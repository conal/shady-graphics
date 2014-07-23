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
  , surfBProg
  , wrapSurfForEffect
  , wrapSurf
  -- * unused but exported to suppress "unused" warning
  , wrapSurfExact
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
import Shady.CompileEs ((:->)(ShaderVF),ShaderVF, GLSL, shaderProgram)  -- ,compile


import Shady.ParamSurf (T, SurfD, surfVN, rotateByMatrix, onX, onY, onZ, translate)
import Shady.Lighting -- (View,Shader)


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

type Zoom  = R1

-- | Surface wrapper for Effects (see Shady.CompileEffect)
type SurfWrapperEffect u' = (u' -> FullSurf) -> (U u' -> ShSurf)

-- | Surface wrapper, e.g., 'wrapSurfExact', 'wrapSurfIN', 'wrapSurfIC'
type SurfWrapper u' = (u' -> FullSurf) -> (u' -> ShSurf)

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
wrapSurfForEffect :: forall u'. EyePosE -> SurfWrapperEffect u'
wrapSurfForEffect eyePos f = liftA2 ShaderVF vert frag
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

-- | Wrap up a parameterized surface for compiling.  Computes normals and
-- lighting per pixel -- sometimes called "exact shading".
wrapSurfExact :: forall u'. EyePosE -> SurfWrapper u'
wrapSurfExact eyePos f = liftA2 ShaderVF vert frag
 where
   vert :: u' -> Point -> (E R4, (Point, E R3))
   vert u' p' = (vTrans (pos <+> 1), (p',pos))
    where
      (_,_,surfd,_) = f u'
      (posF,_) = splitF (surfVN surfd)
      pos = posF (toE p')
   
   frag :: u' -> (Point,E R3) -> (E R4,())
   frag u' (p',pos) = (col, ())
    where
      (l,view,surfd,img) = f u'
      (_,norF) = splitF (surfVN surfd)
      col = colorToR4 (l (view eyePos) (SurfInfo pos (nTrans nor) (img p')))
      nor = norF (toE p')

-- | Wrap up a parameterized surface for compiling.  
-- This variant interpolates normals, as in Phong shading.
wrapSurfIN :: forall u'. EyePosE -> SurfWrapper u'
wrapSurfIN eyePos f = liftA2 ShaderVF vert frag
 where
   vert :: u' -> Point -> (E R4, (Point, (E R3, E R3)))
   vert u' p' = (vTrans (pos <+> 1), (p',(pos,nTrans nor)))
    where
      (_,_,surfd,_) = f u'
      (posF,norF) = splitF (surfVN surfd)
      pos = posF p
      nor = norF p
      p   = toE  p'
   
   frag :: u' -> (Point,(E R3, E R3)) -> (E R4,())
   frag u' (p',(pos,nor)) = (col, ())
    where
      (sh,view,_,img) = f u'
      col = colorToR4 (sh (view eyePos) (SurfInfo pos nor (img p')))

-- TODO: wrapSurfIC, interpolating colors, as in Gouraud shading.

-- | Wrap up a parameterized surface for compiling.  
-- This variant interpolates normals, as in Phong shading.
wrapSurfIC :: forall u'. EyePosE -> SurfWrapper u'
wrapSurfIC eyePos f = liftA2 ShaderVF vert frag
 where
   vert :: u' -> Point -> (E R4, E R4)
   vert u' p' = (vTrans (pos <+> 1), col)
    where
      (sh,view,surfd,img) = f u'
      (posF,norF) = splitF (surfVN surfd)
      pos = posF p
      nor = norF p
      p   = toE  p'
      col = colorToR4 (sh (view eyePos) (SurfInfo pos (nTrans nor) (img p')))
   
   frag :: u' -> E R4 -> (E R4,())
   frag _ col = (col, ())

-- | 3D animation
type SurfB = T -> FullSurf

-- | Surface shader program
surfBProg :: EyePosE -> SurfB -> GLSL R1 R2
surfBProg eyePos s = shaderProgram (wrapSurf eyePos (s . pureD))

