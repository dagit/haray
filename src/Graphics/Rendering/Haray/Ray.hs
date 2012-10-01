module Graphics.Rendering.Haray.Ray where

import Numeric.LinearAlgebra.Vector

data Ray a = Ray
  { rayOrigin    :: !(Vec3 a)
  , rayDirection :: !(Vec3 a)
  } deriving (Read, Show, Eq, Ord)

{-# INLINE pointAtParameter #-}
pointAtParameter :: Floating a => Ray a -> a -> Vec3 a
pointAtParameter (Ray o d) t = o <+> (t*>d)
