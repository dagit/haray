module Data.Texture where

import Data.VectorSpace
import Data.RGB

type Texture a = Vec2 a -> Vec3 a -> RGB a

data MatteData a = MatteData
  { mColor :: RGB a
  } deriving (Read, Show, Eq, Ord)

mkMatteTexture :: MatteData a -> Texture a
mkMatteTexture (MatteData rgb) = \_ _ -> rgb

mkStripeTexture :: (Ord a, Floating a) => Texture a
mkStripeTexture = \_ (Vec3 x y z) ->
  if sin x > 0
    then Vec3 0 0 0
    else Vec3 1 1 1
