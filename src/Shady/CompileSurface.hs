{-# LANGUAGE TypeOperators, ScopedTypeVariables
           , FlexibleContexts, TypeFamilies
  #-}
{-# OPTIONS_GHC -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      :  Shady.CompileSurface
-- Copyright   :  (c) Conal Elliott 2009
-- License     :  AGPLv3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
--
-- Assemble shaders and display an image
----------------------------------------------------------------------

module Shady.CompileSurface
  ( EyePosE, FullSurf
  , SurfB
--  , surfBProg
  , wrapSurf
  -- * unused but exported to suppress "unused" warning
  , wrapSurfExact--, wrapSurfIN, wrapSurfIC
  -- types
  , Zoom
  ) where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&))

import Control.Compose (result)

import Data.Derivative (pureD)

import Shady.Language.Exp hiding (indices)
import Shady.Language.GLSL hiding (Shader)
import Shady.Color (colorToR4)
import Shady.Image (Point,Image)
import Shady.Color (Color)
import Shady.CompileEs
  ((:->)(ShaderVF),ShaderVF,shaderProgram,GLSL)  -- ,compile

import Shady.ParamSurf (T, SurfD, surfVN, rotateByMatrix, onX, onY, onZ, translate)
import Shady.Lighting -- (View,Shader)
import Shady.Complex

-- Arbitrary for now.  Later do progressive (infinite sequence of grids)
-- and adpative.

-- -- # of samples, vertically and horizontally
-- rows, cols :: GlIndex
-- rows = 100
-- cols = 100

-- | Eye position as a single expression.  See also 'EyePos'.
type EyePosE = R3E

-- | Renderable surface
type FullSurf = (Lighter Color, EyePosE -> View, SurfD, Image Color)

splitF :: (a -> (b,c)) -> (a -> b, a -> c)
splitF = result fst &&& result snd

-- | Surface shader.  Vertex stage converts uv into (uv,pos) for
-- fragment stage, which computes normals & lighting per pixel.  Function
-- of eye position.
type ShSurf = ShaderVF Point

--
-- The rotation vectors, pan and zoom values must be provided by the display
-- environment via a GLSL uniform.
-- Thus there is a notion of custom uniform values and required uniform values.
--
-- The U type extends a type representing custom uniforms to one representing
-- the required uniforms as well.
--
type CustomUniforms = (E R3, (E R3, (E R3, (E R3, E Zoom))))

type U u' = (u', CustomUniforms)

type Angle = R1
type Zoom  = R1

-- | Surface wrapper, e.g., 'wrapSurfExact', 'wrapSurfIN', 'wrapSurfIC'
type SurfWrapper u' = (u' -> FullSurf) -> (U u' -> ShSurf)

wrapSurf :: forall u'. EyePosE -> SurfWrapper u'
wrapSurf = wrapSurfExact  -- exact lighting (beautiful)
-- wrapSurf = wrapSurfIN  -- interpolate normals (faster)
-- wrapSurf = wrapSurfIC  -- interpolate colors (terrible)

-- Change wrapSurf to wrapSurfIN or wrapSurfIC to compare.

rotatePanZoom :: CustomUniforms -> SurfD -> SurfD
rotatePanZoom (aRow, (bRow, (cRow, (pan, z)))) surfd =
  \p -> onX (translate panX) .
        onY (translate panY) .
        onZ (translate panZ) .
        rotateByMatrix (conv aRow, conv bRow, conv cRow) .
        (/(pureD z, pureD z, pureD z)) $ surfd p
  where
    (panX, panY, panZ) = conv pan
    conv :: E R3 -> (T,T,T)
    conv v = (pureD (getX v), pureD (getY v), pureD (getZ v))

-- | Wrap up a parameterized surface for compiling.  Computes normals and
-- lighting per pixel -- sometimes called "exact shading".
wrapSurfExact :: forall u'. EyePosE -> SurfWrapper u'
wrapSurfExact eyePos f = liftA2 ShaderVF vert frag
 where
   vert :: U u' -> Point -> (E R4, (Point, E R3))
   vert (u',ru) p' = (vTrans (pos <+> 1), (p',pos))
    where
      surfd = rotatePanZoom ru surfd'
      (_,_,surfd',_) = f u'
      (posF,_) = splitF (surfVN surfd)
      pos = posF (toE p')

   frag :: U u' -> (Point,E R3) -> (E R4,())
   frag (u',ru) (p',pos) = (col, ())
    where
      surfd = rotatePanZoom ru surfd'
      (l,view,surfd',img) = f u'
      (_,norF) = splitF (surfVN surfd)
      col = colorToR4 (l (view eyePos) (SurfInfo pos (nTrans nor) (img p')))
      nor = norF (toE p')

-- -- | Wrap up a parameterized surface for compiling.
-- -- This variant interpolates normals, as in Phong shading.
-- wrapSurfIN :: forall u'. EyePosE -> SurfWrapper u'
-- wrapSurfIN eyePos f = liftA2 ShaderVF vert frag
--  where
--    vert :: U u' -> Point -> (E R4, (Point, (E R3, E R3)))
--    vert (u',z') p' = (vTrans (pos <+> z'), (p',(pos,nTrans nor)))
--     where
--       (_,_,surfd,_) = f u'
--       (posF,norF) = splitF (surfVN surfd)
--       pos = posF p
--       nor = norF p
--       p   = toE  p'
--
--    frag :: U u' -> (Point,(E R3, E R3)) -> (E R4,())
--    frag (u',_) (p',(pos,nor)) = (col, ())
--     where
--       (sh,view,_,img) = f u'
--       col = colorToR4 (sh (view eyePos) (SurfInfo pos nor (img p')))
--
-- -- TODO: wrapSurfIC, interpolating colors, as in Gouraud shading.
--
-- -- | Wrap up a parameterized surface for compiling.
-- -- This variant interpolates normals, as in Phong shading.
-- wrapSurfIC :: forall u'. EyePosE -> SurfWrapper u'
-- wrapSurfIC eyePos f = liftA2 ShaderVF vert frag
--  where
--    vert :: U u' -> Point -> (E R4, E R4)
--    vert (u',z') p' = (vTrans (pos <+> z'), col)
--     where
--       (sh,view,surfd,img) = f u'
--       (posF,norF) = splitF (surfVN surfd)
--       pos = posF p
--       nor = norF p
--       p   = toE  p'
--       col = colorToR4 (sh (view eyePos) (SurfInfo pos (nTrans nor) (img p')))
--
--    frag :: U u' -> E R4 -> (E R4,())
--    frag _ col = (col, ())


-- | 3D animation
type SurfB = T -> FullSurf

--
-- sseefried: In this extended comment I'm going to explain, in a little
-- more detail, just what is going on in 'wrapSurfExact'.
-- I'll digress into talking about how GLSL works where required.
--
-- 'wrapSurfExact' takes an eye position ('eyePos') and a function 'f'
-- of type 'u\' -> FullSurf'. 'wrapSurfExact' has been written to be
-- be polymorphic but in practice the u' parameter is time.
--
-- In the generated GLSL code the u' parameter will appear as the declaration:
--
-- uniform float _uniform;
--
-- (and as an OpenGL or WebGL programmer you are responsible for passing this value
--  into the vertex shader using the OpenGL/WebGL function 'uniform1f')
--
-- We construct two higher order functions called 'vert' and 'frag'.
--
-- Function 'vert'
-- ~~~~~~~~~~~~~~~
-- Function 'vert' takes the uniform 'u\'' and a point 'p\''. Function 'f' is
-- applied to 'u\'' to yield a value of type FullSurf. This is a quadruple of
-- lighting, eyepos to view function, surface and image. We pull out the image as
-- 'surfd'.
--
-- We then transform the surface of vertices to a surface of vertices and normals by
-- applying 'surfVN' to it.
--
-- We then apply 'splitF' to split out the vertex and normal functions
-- (They are both functions from u' -> <something>).
--
-- The vertex function is then applied to 'toE p\'' to yield the surface vertex position.
-- Call this 'pos'.
--
-- Finally 'vert' yields '(vTrans (pos <+> 1), (p\', pos))'. The second component of the
-- pair are the GLSL "varying" values that are passed from the vertex shader to
-- the fragment shader by GLSL's run-time.
--
-- (The Shady data type ShaderVF has been cleverly defined so that the output "varying" values
-- of the vertex shader must have the same time as the input "varying" values of the
-- fragement shader.)
--
-- vTrans (pos <+> 1) produces an expression that when compiled to GLSL looks like
--
-- gl_ModelViewProjectionMatrix * (<x>,<y>,<z>,1.0)
--
--   where <x>, <y>, <z> are expressions representing the x,y,z values of 'pos' above.
--
-- Function 'frag'
-- ~~~~~~~~~~~~~~~
--

