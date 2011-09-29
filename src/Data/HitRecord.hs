module Data.HitRecord where

import Data.VectorSpace
import Data.RGB

data HitRecord a = HitRecord
  { hrT      :: !a
  , hrNormal :: !(Vec3 a)
  , hrColor  :: !(RGB a)
  } deriving (Read, Show, Eq, Ord)
