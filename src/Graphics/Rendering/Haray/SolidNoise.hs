{-# LANGUAGE BangPatterns #-}
module Graphics.Rendering.Haray.SolidNoise
( SolidNoise
, mkSolidNoise
, omega
, gamma
, intGamma
, knot
, turbulence
, dturbulence
, noise
) where

import Numeric.LinearAlgebra.Vector hiding (Vector)

import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ( Unbox )
import Data.List ( foldl1' )

import Control.Monad.Primitive
import System.Random.MWC

data SolidNoise a = SolidNoise
  { snGradient :: !(V.Vector (Vec3 a)) -- TODO: smart constructor alert: this is meant to have length 16
  , snPhi      :: !(V.Vector Int)
  } deriving (Read, Show, Eq, Ord)

gradientSize :: Int
gradientSize = 16

-- | Computes w(t) = -6|t|^6 + 15|t|^4 - 10|t|^3 + 1
-- and assumes that t in [-1, 1]
omega :: (Ord a, Floating a) => a -> a
omega t | t < 0 = omega (negate t)
omega t         = (-6)*t*t*t3 + 15*t*t3 + (-10)*t3 + 1
  where
  !t3 = t*t*t
{-# SPECIALIZE INLINE omega :: Double -> Double #-}
{-# SPECIALIZE INLINE omega :: Float  -> Float  #-}

gamma :: (RealFloat a, Unbox a, Floating a, Ord a) => SolidNoise a -> Int -> Int -> Int -> Vec3 a
gamma sn !i !j !k = (snGradient sn) V.! idx2
  where
  !phi  = snPhi sn
  !idx0 = abs k `mod` gradientSize
  !idx1 = phi V.! (abs (j + idx0) `mod` gradientSize)
  !idx2 = phi V.! (abs (i + idx1) `mod` gradientSize)
{-# SPECIALIZE INLINE gamma :: SolidNoise Double -> Int -> Int -> Int -> Vec3 Double #-}
{-# SPECIALIZE INLINE gamma :: SolidNoise Float  -> Int -> Int -> Int -> Vec3 Float #-}

intGamma :: (Floating a, Ord a) => SolidNoise a -> Int -> Int -> Int
intGamma sn !i !j = idx2
  where
  !phi  = snPhi sn
  !idx0 = abs j `mod` gradientSize
  !idx1 = phi V.! idx0
  !idx2 = phi V.! (abs (i + idx1) `mod` gradientSize)
{-# SPECIALIZE INLINE intGamma :: SolidNoise Double -> Int -> Int -> Int #-}
{-# SPECIALIZE INLINE intGamma :: SolidNoise Float  -> Int -> Int -> Int #-}

knot :: (RealFloat a, Unbox a, Floating a, Ord a) => SolidNoise a -> Int -> Int -> Int -> Vec3 a -> a
knot sn !i !j !k v@(Vec3 x y z) = omega x * omega y * omega z * (gamma sn i j k <.> v)
{-# SPECIALIZE INLINE knot :: SolidNoise Double -> Int -> Int -> Int -> Vec3 Double -> Double #-}
{-# SPECIALIZE INLINE knot :: SolidNoise Float  -> Int -> Int -> Int -> Vec3 Float  -> Float #-}

mkSolidNoise :: (Unbox a, RealFloat a, Num a, PrimMonad m)
             => Gen (PrimState m) -> m (SolidNoise a)
mkSolidNoise gen = do
  let v = V.fromList [Vec3 1 1 0, Vec3 (-1)    1 0, Vec3 1 (-1)    0, Vec3 (-1) (-1)    0
                     ,Vec3 1 0 1, Vec3 (-1)    0 1, Vec3 1    0 (-1), Vec3 (-1)    0 (-1)
                     ,Vec3 0 1 1, Vec3    0 (-1) 1, Vec3 0    1 (-1), Vec3 0    (-1) (-1)
                     ,Vec3 1 1 0, Vec3 (-1)    1 0, Vec3 0 (-1)    1, Vec3 0    (-1) (-1)]
      phi = [0..gradientSize-1]
  phi' <- shuffle phi
  return $! SolidNoise { snGradient = v, snPhi = V.fromList phi' }
  where
  -- This is slightly inefficient in terms of list operations, but our lists are only
  -- length 16 so it seems like overkill to use other data structures.
  -- fetch :: [a] -> ST s (a,[a])
  fetch []  = error "Graphics.Rendering.Haray.SolidNoise.fetch: called on []"
  fetch [x] = return (x,[])
  fetch xs  = do
    i <- uniformR (1, length xs) gen
    let (h,tl) = splitAt i xs
    return (last h, init h ++ tl)
  
  -- shuffle :: [a] -> ST s [a]
  shuffle [] = return []
  shuffle xs = do
    (x,xs') <- fetch xs
    ys      <- shuffle xs'
    return $! x : ys
{-# SPECIALIZE mkSolidNoise :: (PrimMonad m) => Gen (PrimState m) -> m (SolidNoise Double) #-}
{-# SPECIALIZE mkSolidNoise :: (PrimMonad m) => Gen (PrimState m) -> m (SolidNoise Float)  #-}

turbulence :: (RealFloat a, Unbox a, RealFrac a, Floating a) => SolidNoise a -> Vec3 a -> Int -> a
turbulence _  _ depth | depth < 1 = error "Graphics.Rendering.Haray.SolidNoise.turbulence: depth must be > 0"
turbulence sn !p depth = sum' (zipWith (/) ns twos)
  where
  twos = map (2^) [0..depth-1]
  ps   = zipWith (*>) twos (repeat p)
  ns   = map (abs . (noise sn)) ps
{-# SPECIALIZE INLINE turbulence :: SolidNoise Double -> Vec3 Double -> Int -> Double #-}
{-# SPECIALIZE INLINE turbulence :: SolidNoise Float  -> Vec3 Float  -> Int -> Float  #-}

dturbulence :: (RealFloat a, Unbox a, RealFrac a, Floating a) => SolidNoise a -> Vec3 a -> Int -> a -> a
dturbulence _  _ depth _ | depth < 1 = error "Graphics.Rendering.Haray.SolidNoise.dturbulence: depth must be > 0"
dturbulence sn !p depth d = sum' (zipWith (/) ns ds)
  where
  ds = map (d^) [0..depth-1]
  ps = zipWith (*>) ds (repeat p)
  ns = map (abs . (noise sn)) ps
{-# SPECIALIZE INLINE dturbulence :: SolidNoise Double -> Vec3 Double -> Int -> Double -> Double #-}
{-# SPECIALIZE INLINE dturbulence :: SolidNoise Float  -> Vec3 Float  -> Int -> Float  -> Float  #-}

sum' :: Num a => [a] -> a
sum' = foldl1' (+) -- faster than Perlude.sum
{-# SPECIALIZE INLINE sum' :: [Double] -> Double #-}
{-# SPECIALIZE INLINE sum' :: [Float]  -> Float  #-}
{-# SPECIALIZE INLINE sum' :: [Int]    -> Int    #-}

noise :: (RealFloat a, Unbox a, RealFrac a, Floating a) => SolidNoise a -> Vec3 a -> a
noise sn (Vec3 x y z) = 
  let !fi  = floor x
      !fj  = floor y
      !fk  = floor z
      !fu  = x - fromIntegral fi
      !fv  = y - fromIntegral fj
      !fw  = z - fromIntegral fk
  -- This is much faster than using sum or sum'
  in knot sn fi     fj     fk     (Vec3 fu     fv     fw)     +
     knot sn (fi+1) fj     fk     (Vec3 (fu-1) fv     fw)     +
     knot sn fi     (fj+1) fk     (Vec3 fu     (fv-1) fw)     +
     knot sn fi     fj     (fk+1) (Vec3 fu     fv     (fw-1)) +
     knot sn (fi+1) (fj+1) fk     (Vec3 (fu-1) (fv-1) fw)     +
     knot sn (fi+1) fj     (fk+1) (Vec3 (fu-1) fv     (fw-1)) +
     knot sn fi     (fj+1) (fk+1) (Vec3 fu     (fv-1) (fw-1)) +
     knot sn (fi+1) (fj+1) (fk+1) (Vec3 (fu-1) (fv-1) (fw-1))
{-# SPECIALIZE noise :: SolidNoise Double -> Vec3 Double -> Double #-}
{-# SPECIALIZE noise :: SolidNoise Float  -> Vec3 Float  -> Float  #-}
