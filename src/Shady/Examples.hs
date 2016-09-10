{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-} -- TEMP
-- {-# OPTIONS_GHC -fno-warn-unused-binds   #-} -- TEMP

----------------------------------------------------------------------
-- |
-- Module      :  Shady.Examples
-- Copyright   :  (c) Conal Elliott
-- 
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
-- 
-- Examples
----------------------------------------------------------------------

module Shady.Examples where

-- TODO: explicit exports

import Prelude hiding ((<*))

import Control.Applicative ((<$>),(<*>), liftA2)
import System.Directory (createDirectoryIfMissing)

import Control.Compose (result)

import Data.Boolean

import Data.VectorSpace
-- import Data.Derivative (pureD)

import Shady.Complex
import Shady.Misc (frac)
import Shady.Language.Exp
import Shady.Color
import Shady.Image
-- import Shady.ITransform
-- import Shady.Lighting (view1,intrinsic)
-- import Shady.ParamSurf (xyPlane,SurfD,T)
import Shady.CompileE (GLSL(..))
import Shady.CompileImageSimple (ImageB,imageBProg)
-- import Shady.CompileSurface (FullSurf)

type Sink a = a -> IO ()

-- Testing
x :: HasColor c => Sink (ImageB c)
x = print . imageBProg

saveShader :: String -> String -> String -> IO ()
saveShader suffix name content =
  do createDirectoryIfMissing False "sample-shaders"
     writeFile ("sample-shaders/" ++ name ++ "." ++ suffix) content

saveVert, saveFrag, saveSh :: String -> Sink (GLSL R1 R2)

saveVert name (GLSL v _ _ _) = saveShader "vert" name v
saveFrag name (GLSL _ f _ _) = saveShader "frag" name f

saveSh name glsl = saveVert name glsl >> saveFrag name glsl

-- Save a fragment shader.  Don't bother with the vertex shader, since
-- they're all the same.
saveIm :: HasColor c => String -> Sink (ImageB c)
saveIm name = saveFrag name . imageBProg

-- Shared image vertex shader.  
saveImVert :: IO ()
saveImVert = saveVert "image" (imageBProg a0)

-- | Animated region
type RegionB = ImageB BoolE

a0,a1' :: RegionB
a0  t = uscale2 (sin t + 1.05) checker
a1' t = rotate2 t checker

a1,a2,a3 :: RegionB
a1 t = rotate2 t $ a0 t
a2 t = uscale2 (cos t) udisk
a3 = (liftA2.liftA2) (==*) a1 a2

a4,a5,a6 :: ImageB Color
a4 = (fmap.fmap) (boolean blue red) a1
a5 = (fmap.fmap) (boolean (blue ^/ 2) clear) a2
a6 = liftA2 over a5 a4

realPos :: Region
realPos = (>* 0) . realPart

imagPos :: Region
imagPos = (>* 0) . imagPart

a7, a8 :: ImageB Color
a7 = (fmap.fmap) (boolean clear (black ^/ 2)) a2  -- blank on its own
a8 = liftA2 over a7 ((fmap.fmap) (boolean blue red) a1)

a9a, a9b, a9c :: RegionB
a9a t = a0 t . sin
a9b t = a0 t . cos
a9c t = a0 t . tan

a9d :: ImageB BoolE
a9d t = (>* sin t) . magnitude . cos

a10 :: ImageB Color
a10 t = rotate2 t $ uscale2 (sin t) $
        bilerpC red blue black white

a11 t = a5 t `over` a10 t

a12 t = fmap toColor (a1 t) `over` a10 t

a13 t = swirl (sin (0.3 * t)) $
        uscale2 (1.05 + cos (0.7 * t)) checker

a14 = intersectR udisk <$> a13

a15 t = uscale2 (1 + cos t / 2) utile (a10 t)

stripes (a :+ _) = frac a <* 0.5

a16 t = swirl (sin t / 3) stripes

a17 t = swirl (sin t / 5) (a15 t)

a18 _ = utile (disk 0.45)
a19 t = tile (w :+ w) (disk rad)
 where
   w   = 1 + sin (1.5 * t) / 7
   rad = 0.3 + cos (1.1 * t) / 9
a19b = rotate2 <*> a19

-- | Swirl transformation
-- swirl' :: ITrans Point a => FloatE -> Filter a
swirl' s = rotate2Im ((2*pi* sin s*) . magnitude)

a19c = swirl  <*> a19b
a19d = swirl' <*> a19b
a19e = (uscale2 . (1.1 +) . cos . (+ pi/3) . (/ 2)) <*> a19d

a19f t = uscale2 (1.1 + cos (t/2 + pi/3)) (a19d t)
a19g t = uscale2 (cos t) (a19d t)
a19h t = uscale2 t (a19d t)
a19i t = translate2 (t:+t) (a19d t)

a19j :: RegionB
a19j t = translate2 (cis (t/5)) (a19 0)

a19k :: RegionB
a19k t = translate2 (t :+ 0) . uscale2 (1/4) $ a19 (pi/3)

lerpL lo hi t = lerp lo hi ((1 + cos t) / 2)

diskL :: FloatE -> FloatE -> FloatE -> Region
-- diskL lo hi t = disk (lerpL lo hi t)
diskL = (result.result.result) disk lerpL

a20a = diskL 0 1

a20 = utile . diskL 0.3 0.5

a20b t = translate2 (cis (t/5)) . uscale2 (1/2) $ a20 t 

a20c = utile . diskL 0 (sqrt 2 / 2)

a21 lo hi t = utile (uscale2 (lerpL lo hi t) $ annulus (1/2) (1/4))

a21a, a21b :: RegionB
a21a = a21 0 1
a21b = a21 (1/3) (2/3)

a21c = (result.result) (boolean red blue) a21a

-- wedges :: IntE -> Region
-- wedges n = 

a22a = (swirl . sin) <*> a21b
a22b = (swirl . (* 0.2) . sin) <*> a21b

{--------------------------------------------------------------------
    Textures
--------------------------------------------------------------------}


-- -- Tweak for -1 to 1, and Y inversion.
-- samplerIm' :: Sampler N2 :=> Image Color
-- samplerIm' = translate2 (d:+d) . scale2 (s :+ (-s)) . samplerIm
--  where
--    d = -1
--    s =  2

-- samplerIn' :: In (Sampler N2)
-- samplerIn' = title "texture" samplerIn

{--------------------------------------------------------------------
    Compiling examples
--------------------------------------------------------------------}

saveAll :: IO ()
saveAll = do saveImVert
             saveIm "a0" a0
             saveIm "a1" a1
             saveIm "a2" a2
             saveIm "a3" a3
             saveIm "a4" a4
             saveIm "a5" a5
             saveIm "a6" a6
             saveIm "a8" a8
             saveIm "a9b" a9b
             saveIm "a9c" a9c
             saveIm "a10" a10
             saveIm "a12" a12
             saveIm "a13" a13
             saveIm "a15" a15
             saveIm "a16" a16
             saveIm "a17" a17
             saveIm "a19" a19
             saveIm "a19b" a19b
             saveIm "a19c" a19c
             saveIm "a19d" a19d
             saveIm "a19e" a19e
             saveIm "a19j" a19j
             saveIm "a20c" a20c
             saveIm "a21a" a21a
             saveIm "a22b" a22b

-- main = run a22b
