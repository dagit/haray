{-# LANGUAGE BangPatterns #-}
module Graphics.Rendering.Haray.Texture where

import Numeric.LinearAlgebra.Vector
import Graphics.Rendering.Haray.RGB
import Graphics.Rendering.Haray.SolidNoise
import Data.Vector.Unboxed

import Control.Monad.Primitive
import System.Random.MWC

type Texture a = Vec2 a -> Vec3 a -> RGB a

data MatteData a = MatteData
  { mColor :: RGB a
  } deriving (Read, Show, Eq, Ord)

mkMatteTexture :: MatteData a -> Texture a
mkMatteTexture (MatteData rgb) = \_ _ -> rgb

mkStripeTexture :: (Ord a, Floating a) => Texture a
mkStripeTexture = \_ (Vec3 x _ _) ->
  if sin x > 0
    then Vec3 0 0 0
    else Vec3 1 1 1

data NoiseData a = NoiseData
  { ndScale      :: !a
  , ndColor0     :: !(RGB a)
  , ndColor1     :: !(RGB a)
  , ndSolidNoise :: !(SolidNoise a)
  } deriving (Read, Show, Eq, Ord)

mkNoiseTexture :: (Unbox a, RealFloat a, RealFrac a, Floating a) => NoiseData a -> Texture a
mkNoiseTexture (NoiseData scale c0 c1 sn) =
  \_ p -> let !t = (1 + (noise sn (scale *> p))) / 2
          in (t*>c0) <+> ((1 - t)*>c1)
{-# SPECIALIZE mkNoiseTexture :: NoiseData Double -> Texture Double #-}
{-# SPECIALIZE mkNoiseTexture :: NoiseData Float  -> Texture Float  #-}

mkBWNoiseTexture :: (Unbox a, RealFloat a, RealFrac a, Floating a, PrimMonad m)
                 => Gen (PrimState m) -> m (Texture a)
mkBWNoiseTexture gen = do
  sn <- mkSolidNoise gen
  return (mkNoiseTexture
           (NoiseData
             { ndScale      = 0.05
             , ndColor0     = Vec3 1.0 1.0 1.0
             , ndColor1     = Vec3 0.0 0.0 0.0
             , ndSolidNoise = sn }))
{-# SPECIALIZE mkBWNoiseTexture :: (PrimMonad m) => Gen (PrimState m) -> m (Texture Double) #-}
{-# SPECIALIZE mkBWNoiseTexture :: (PrimMonad m) => Gen (PrimState m) -> m (Texture Float)  #-}

data MarbleData a = MarbleData
  { mdFreq       :: !a
  , mdScale      :: !a
  , mdOctaves    :: !Int
  , mdColor0     :: !(RGB a)
  , mdColor1     :: !(RGB a)
  , mdColor2     :: !(RGB a)
  , mdSolidNoise :: !(SolidNoise a)
  } deriving (Read, Show, Eq, Ord)

mkMarbleData :: (RealFloat a, Unbox a, Floating a, PrimMonad m)
             => Gen (PrimState m) -> a -> m (MarbleData a)
mkMarbleData gen stripes_per_unit = do
  sn <- mkSolidNoise gen
  return (MarbleData
           { mdFreq       = pi * stripes_per_unit
           , mdScale      = 5
           , mdOctaves    = 8
           , mdColor0     = Vec3 0.8  0.8  0.8
           , mdColor1     = Vec3 0.4  0.2  0.1
           , mdColor2     = Vec3 0.06 0.04 0.02
           , mdSolidNoise = sn })
{-# SPECIALIZE INLINE mkMarbleData :: (PrimMonad m) => Gen (PrimState m) -> Double -> m (MarbleData Double) #-}
{-# SPECIALIZE INLINE mkMarbleData :: (PrimMonad m) => Gen (PrimState m) -> Float  -> m (MarbleData Float)  #-}

mkMarbleTexture :: (RealFloat a, Unbox a, Floating a, RealFrac a) => MarbleData a -> Texture a
mkMarbleTexture md =
  \_ p@(Vec3 x _ _) -> let !temp = mdScale md * turbulence (mdSolidNoise md) ((mdFreq md) *> p) (mdOctaves md)
                           !t    = 2 * abs (sin ((mdFreq md) * x + temp))
                       in if (t < 1)
                            -- TODO: refactor this to have a linear interpolation function
                            then (t *> mdColor1 md) <+> ((1 - t) *> mdColor2 md)
                            else let !t' = t - 1
                                 in (t' *> mdColor0 md) <+> ((1 - t') *> mdColor1 md)
{-# SPECIALIZE mkMarbleTexture :: MarbleData Double -> Texture Double #-}
{-# SPECIALIZE mkMarbleTexture :: MarbleData Float  -> Texture Float  #-}
