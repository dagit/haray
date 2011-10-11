module Data.Texture where

import Data.VectorSpace
import Data.RGB

type Texture a = Vec2 a -> Vec3 a -> RGB a

data MatteData a = MatteData
  { mColor :: RGB a
  } deriving (Read, Show, Eq, Ord)

mkMatteTexture :: MatteData a -> Texture a
mkMatteTexture (MatteData rgb) = \_ _ -> rgb
