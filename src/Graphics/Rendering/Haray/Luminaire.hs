module Data.Luminaire where

import Data.VectorSpace
import Data.RGB

data DirectedLight a = DirectedLight
  { dlDirection :: !(Vec3 a)
  , dlColor     :: !(RGB a)
  } deriving (Read, Show, Eq, Ord)

data AmbientLight a = AmbientLight
  { alColor :: !(Vec3 a) } deriving (Read, Show, Eq, Ord)
