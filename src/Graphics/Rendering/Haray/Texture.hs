{-# LANGUAGE BangPatterns #-}
module Graphics.Rendering.Haray.Texture where

import Numeric.LinearAlgebra.Vector
import Graphics.Rendering.Haray.RGB
import Graphics.Rendering.Haray.SolidNoise

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
  { ndScale      :: a
  , ndColor0     :: RGB a
  , ndColor1     :: RGB a
  , ndSolidNoise :: SolidNoise a
  } deriving (Read, Show, Eq, Ord)

mkNoiseTexture :: (RealFrac a, Floating a) => NoiseData a -> Texture a
mkNoiseTexture (NoiseData scale c0 c1 sn) =
  \_ p -> let !t = (1 + (noise sn (scale *> p))) / 2
          in (t*>c0) <+> ((1 - t)*>c1)

mkBWNoiseTexture :: (RealFrac a, Floating a) => IO (Texture a)
mkBWNoiseTexture = do
  sn <- mkSolidNoise
  return (mkNoiseTexture
           (NoiseData
             { ndScale      = 0.05
             , ndColor0     = Vec3 1.0 1.0 1.0
             , ndColor1     = Vec3 0.0 0.0 0.0
             , ndSolidNoise = sn }))
