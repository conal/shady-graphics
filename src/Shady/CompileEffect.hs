{-# LANGUAGE RankNTypes, GADTs, TypeFamilies, ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Shady.CompileEffect(
  -- data types
  ShadyEffect(..), ShadyGeometry(..), WebGLEffect {-opaque-}, UIElem {-opaque-}, UI {-opaque-}, 
  -- smart constructors for UIElem
  uiTime, uiSliderF, uiSliderWithStepF, uiSliderI, {- monad instance -}
  -- smart constructors for ShadyEffect and ShadyGeometry record types
  shadyEffect, shadyGeometry,
  -- functions that operate on opaque data type WebGLEffect
  compileEffect,                       -- constructs   WebGLEffect
  fragmentShader, vertexShader, jsonUI -- deconstructs WebGLEffect
) where

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


--
-- | A selector function for extracting fragment shader from WebGLEffect
--
fragmentShader  :: WebGLEffect -> String
fragmentShader (WebGLEffect (GLSL _ fs _ _) uniforms _) = toShader uniforms fs

--
-- | A selector function for extracting vertex shader from WebGLEffect
--
vertexShader :: WebGLEffect -> String
vertexShader (WebGLEffect (GLSL vs _ _ _) uniforms _) = toShader uniforms vs

--
-- | Converts a WebGLEffect into a JSON array containing descriptions of
--   of all the UI elements of the effect.
--
jsonUI :: WebGLEffect -> String
jsonUI (WebGLEffect _ _ jsons) = prettyJA $ JsonArray jsons

--
-- | Compiles a ShadyEffect to a GLSL program.
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

--
-- Smart constructors
--
uiTime :: UI (E (One Float))
uiTime    = UIElem UITime

--
-- | Creates a slider that produces Float values.
--
-- Conditions:
--   minVal <= defaultVal <= maxVal
--
-- If these conditions do not hold then "sensible" values are substituted.
--
uiSliderF :: String -> Float -> Float -> Float -> UI (E (One Float))
uiSliderF title minVal defaultVal maxVal = UIElem (UISliderF title minVal' defaultVal' maxVal')
  where (minVal', defaultVal', maxVal', _) = sensible (minVal, defaultVal, maxVal, 0)

--
-- | Creates a slider that produces Float values with a step value.
--
-- Condtions:
--   minVal <= defaultVal <= maxVal
--   step <= maxVal - minVal
--
-- If these conditions do not hold then "sensible" values are substituted.
--
uiSliderWithStepF :: String -> Float -> Float -> Float -> Float -> UI (E (One Float))
uiSliderWithStepF title minVal defaultVal maxVal step =
  UIElem (UISliderWithStepF title minVal' defaultVal' maxVal' step')
  where
    (minVal', defaultVal', maxVal', step') = sensible (minVal, defaultVal, maxVal, step)

--
-- | Creates a slider that produces Int values.
--
-- Conditions:
--   minVal <= defaultVal <= maxVal
--
-- If these conditions do not hold then "sensible" values are substituted.
--
uiSliderI :: String -> Int -> Int -> Int -> UI (E (One Int))
uiSliderI title minVal defaultVal maxVal =
  UIElem (UISliderI title minVal' defaultVal' maxVal')
  where
    (minVal', defaultVal', maxVal',_) = sensible (minVal, defaultVal, maxVal, 0)

--
-- A helper function to clamp "bad" slider values to sensible ones.
--
-- Examples:
--  sensible (0, 2, -3,   1)  == (0, 0, 0, 0)
--  sensible (0, 5,  3,   1)  == (0, 3, 3, 1)
--  sensible (0, -5, 3,   1)  == (0, 0, 3, 1)
--  sensible (0, 2,  5,  10)  == (0, 2, 5, 5)
--  sensible (0, 2,  5, -10)  == (0, 2, 5, 0)
--
sensible :: (Num a, Ord a) => (a,a,a,a) -> (a,a,a,a)
sensible (minVal, defaultVal, maxVal, step) = (minVal, defaultVal', maxVal', step')
  where
    maxVal'     = minVal `max` maxVal
    defaultVal' = (minVal `max` defaultVal) `min` maxVal'
    step'       = ((maxVal' - minVal) `min` step) `max` 0

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