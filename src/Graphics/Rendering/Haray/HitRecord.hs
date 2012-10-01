module Data.HitRecord where

import Data.VectorSpace
import Data.Texture

data HitRecord a = HitRecord
  { hrT      :: !a
  , hrNormal :: !(Vec3 a)
  , hrUV     :: !(Vec2 a)
  , hrHitP   :: !(Vec3 a)
  , hrHitTex :: !(Texture a)
  }
