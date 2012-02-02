{-# LANGUAGE RankNTypes, GADTs, TypeFamilies, ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Shady.CompileEffect where

-- System libraries
import Data.Monoid
import Control.Monad
import Control.Applicative
import Text.Printf
import Control.Arrow        (second)

-- friends
import Shady.CompileEffect.Json
import Shady.Image          (Image)
import Shady.Color          (Color, HasColor, black, clear, toColor)
import Shady.CompileImage   (eyePos)
import Shady.CompileSurface (wrapSurf, EyePosE)
import Shady.ParamSurf      (SurfD, T, xyPlane)
import Shady.Lighting       (View, view1, basicStd)
import Shady.CompileE       (GLSL(..))
import Shady.CompileEs      (shaderProgram)
import Text.PrettyPrint.Leijen.DocExpr (HasExpr(..))
import Shady.Language.Exp  (R1, R2, R3E, pureE, BoolE, patE, patT, ExpT(..), E(..), 
                            FromE(..), HasType, pat)
import Shady.Vec as V
import Shady.Misc           (EyePos)
import Data.Derivative      (powVal, pureD)
import Data.NameM

data ShadyEffect c = ShadyEffect {
  shadyGeometryUI  :: UI (ShadyGeometry c),
  shadyEyePos      :: EyePos,
  shadyViewGen     :: R3E -> View
}

-- default shadyEffect
shadyEffect :: UI (ShadyGeometry c) -> ShadyEffect c
shadyEffect shadyGeomUI = ShadyEffect {
    shadyGeometryUI  = shadyGeomUI,
    shadyEyePos      = eyePos,
    shadyViewGen     = view1 }

data ShadyGeometry c = ShadyGeometry {
  shadyImage   :: Image c,
  shadySurface :: SurfD
}

shadyGeometry :: ShadyGeometry Color
shadyGeometry = ShadyGeometry {
    shadyImage       = const $ clear,
    shadySurface     = xyPlane }

data UIElem a where
  UISliderF :: String -- ^ title
            -> Float  -- ^ lower bound
            -> Float  -- ^ default
            -> Float  -- ^ upper bound
            -> UIElem (E (One Float))
  UISliderWithStepF :: String -- ^ title
                    -> Float  -- ^ lower bound
                    -> Float  -- ^ default
                    -> Float  -- ^ step (step <= upperbound - lowerbound)
                    -> Float  -- ^ upper bound
                    -> UIElem (E (One Float))
  UISliderI :: String -- ^ title
            -> Int    -- ^ lower bound
            -> Int    -- ^ default
            -> Int    -- ^ upper bound
            -> UIElem (E (One Int))
  UITime    :: UIElem (E (One Float))

data UI a where
  UIElem  :: (FromE a, HasType (ExpT a)) => UIElem a -> UI a
  UIBind  :: UI a -> (a -> UI b) -> UI b
  UIReturn :: a -> UI a

elemName :: forall a.String -> UIElem a -> NameM String
elemName uniquePrefix e = do
  nm <- genName
  let suffix :: String
      suffix = case e of
                 UISliderWithStepF _ _ _ _ _ -> "slider_with_step_f"
                 UISliderF _ _ _ _ -> "slider_f"
                 UISliderI _ _ _ _ -> "slider_i"
                 UITime            -> "time"
  return (printf "%s_%s_%s" uniquePrefix nm suffix)

-- Untyped version of Variables in Shady.Language.Exp
data VU = VU { uVarName :: String, uVarType :: String }

instance Show VU where
  show vu = printf "uniform %s %s" (uVarType vu) (uVarName vu)

runUINameM :: String -> UI a -> NameM (a, [(VU,JsonValue)])
runUINameM uniquePrefix ui = case ui of
  UIReturn a          -> return (a, [])
  UIBind (UIElem e) f -> do
    name <- elemName uniquePrefix e
    let vu = VU name (show (patT p))
        p = pat $ name
    (a, varsAndJsons) <- go . f $ (fromE . patE $ p)
    return $ (a, (vu, uiElemToJson name e):varsAndJsons)
  UIBind ui f -> do
    (a, varsAndJsons)  <- go ui
    (a', varsAndJsons') <- go (f a)
    return (a', varsAndJsons ++ varsAndJsons')
  where
    go = runUINameM uniquePrefix

runUI :: String -> UI a -> (a, [(VU, JsonValue)])
runUI uniquePrefix = runNameM . runUINameM uniquePrefix

instance Monad UI where
  return = UIReturn
  (>>=)  = UIBind

type VertexPosAttribute = R2

data WebGLEffect = WebGLEffect (GLSL () VertexPosAttribute) [VU] [JsonValue]

toShader :: [VU] -> String -> String
toShader uniforms shader = printf "%s\n%s\n%s" shaderHeaders uniformDecs shader
  where
    uniformDecs :: String
    uniformDecs = concatMap (printf "%s;\n" . show) $ uniforms
    shaderHeaders = unlines [
        "#define gl_ModelViewProjectionMatrix ModelViewProjectionMatrix"
      , "#define gl_NormalMatrix NormalMatrix"
      , ""
      , "precision highp float;"
      , ""
      , "uniform mat4 ModelViewProjectionMatrix;"
      , "uniform mat3 NormalMatrix;"
      , ""
      , "#define _attribute mesh_coords"
      , "/* varying_F is just copy of mesh_coords"
      , "   varying_S is vertex position of mesh coordinate after transformation */"
      ]


fragmentShader  :: WebGLEffect -> String
fragmentShader (WebGLEffect (GLSL _ fs _ _) uniforms _) = toShader uniforms fs

vertexShader :: WebGLEffect -> String
vertexShader (WebGLEffect (GLSL vs _ _ _) uniforms _) = toShader uniforms vs

jsonUI :: WebGLEffect -> String
jsonUI (WebGLEffect _ _ jsons) = prettyJA $ JsonArray jsons

--
-- Compiles a ShadyEffect to a GLSL program.
--
-- An OpenGL (or WebGL) program that links to this GLSL program
-- should set up a 'uniform' value denoting the time value of the animation
-- and an 'attribute' denoting the vertex positions.
--
-- (For definitions of 'uniform' and 'attribute' see the GLSL spec)
--
compileEffect :: forall c. (HasColor c) => String -> ShadyEffect c -> WebGLEffect
compileEffect prefix e = WebGLEffect glsl uniforms jsons
  where
    glsl = shaderProgram $ wrapSurf eyePosE (\() -> fullSurf)
    (uniforms,jsons) = unzip uniformsAndJsons
    (geom, uniformsAndJsons) = runUI prefix $ shadyGeometryUI e
    fullSurf = (basicStd, shadyViewGen e, surface, image)
       where
         surface = shadySurface geom
         image   = toColor . shadyImage geom
    eyePosE :: EyePosE
    eyePosE = pureE (V.vec3 ex ey ez) where (ex,ey,ez) = shadyEyePos e

-- Smart constructors
uiTime :: UI (E (One Float))
uiTime    = UIElem UITime

uiSliderF :: String -- ^title
          -> Float  -- from
          -> Float
          -> Float  -- to
          -> UI (E (One Float))
uiSliderF = (((UIElem.).).).UISliderF

uiSliderWithStepF :: String
                  -> Float
                  -> Float
                  -> Float
                  -> Float
                  -> UI (E (One Float))
uiSliderWithStepF = ((((UIElem.).).).).UISliderWithStepF

uiSliderI :: String -- ^title
          -> Int  -- from
          -> Int
          -> Int  -- to
          -> UI (E (One Int))
uiSliderI = (((UIElem.).).).UISliderI

---------------------------------

--
-- The "sort" and "ui-type" attributes are to be used by a Javascript
-- program to correctly render UIs and pull the right type of values
-- out of those UIs.
--
uiElemToJson :: String -> UIElem a -> JsonValue
uiElemToJson uniformName e = addNameEntry $ case e of
    UISliderF title lower defaultVal upper ->
      [ ("sort",      JVString "float_slider")
      , ("ui-type",   JVString "float")
      , ("title",     JVString title)
      , ("min",       JVNumber lower)
      , ("value",     JVNumber defaultVal)
      , ("max",       JVNumber upper) ]
    UISliderWithStepF title lower defaultVal step upper ->
        [ ("sort",      JVString "float_slider_with_step")
        , ("ui-type",   JVString "float")
        , ("title",     JVString title)
        , ("min",       JVNumber lower)
        , ("value",     JVNumber defaultVal)
        , ("step",      JVNumber step)
        , ("max",       JVNumber upper) ]
    UISliderI title lower defaultVal upper ->
      [ ("sort",      JVString "int_slider")
      , ("ui-type",   JVString "int")
      , ("title",     JVString title)
      , ("min",       JVNumber (fromIntegral lower))
      , ("value",     JVNumber (fromIntegral defaultVal))
      , ("max",       JVNumber (fromIntegral upper)) ]
    UITime ->
      [ ("sort",      JVString "time")
      , ("ui-type",   JVString "time") ]
  where
    addNameEntry = JVObject .JsonObject . (("name", JVString uniformName):)

----------------------------------

testEffect = shadyEffect $ do
  t <- uiTime
  f <- uiSliderF "aslider" 1 4 5
  x <- do y <- uiSliderI "slider" 1 9 10
          return (y + 1)
  return $ shadyGeometry