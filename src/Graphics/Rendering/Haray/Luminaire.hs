module Graphics.Rendering.Haray.Luminaire where

import Numeric.LinearAlgebra.Vector
import Graphics.Rendering.Haray.RGB

data DirectedLight a = DirectedLight
  { dlDirection :: !(Vec3 a)
  , dlColor     :: !(RGB a)
  } deriving (Read, Show, Eq, Ord)

data AmbientLight a = AmbientLight
  { alColor :: !(Vec3 a) } deriving (Read, Show, Eq, Ord)
