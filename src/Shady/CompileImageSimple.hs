{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  Shady.CompileImageSimple
-- Copyright   :  (c) 2015 Tabula, Inc.
-- 
-- Maintainer  :  conal@tabula.com
-- Stability   :  experimental
-- 
-- Compile a parametrized image. For now, just animated, i.e., time-dependent.
----------------------------------------------------------------------

module Shady.CompileImageSimple (ImageB, imageBProg) where

-- TODO: explicit exports

import Control.Applicative (liftA2)

-- import TypeUnary.Vec((<+>),vec4)

import Shady.Complex(Complex(..))
import Shady.Language.Type (R1,R2)
import Shady.Language.Exp ((:=>),vec4)
import Shady.Color -- (white,HasColor(..))
import Shady.Image (Image,Point)
import Shady.CompileEs
  ((:-^),(:-*),(:->)(ShaderVF),ShaderVF,shaderProgram,GLSL)  -- ,compile

-- | 2D animation
type ImageB c = R1 :=> Image c

imageBShader :: HasColor c => ImageB c -> (R1 :=> ShaderVF Point)
imageBShader imb = liftA2 ShaderVF vert frag
 where
   vert :: R1 :=> (Point :-^ Point)
   frag :: R1 :=> (Point :-* ())
   vert _t p@(x :+ y) = (vec4 x y 0 1, p)
   frag t p = (colorToR4 (toColor (imb t p)), ())
   -- frag t p = (colorToR4 white, ())

-- | GLSL program for an 'ImageB'.
imageBProg :: HasColor c => ImageB c -> GLSL R1 R2
imageBProg = shaderProgram . imageBShader
