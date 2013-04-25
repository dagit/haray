module Graphics.Rendering.Haray.Camera where

import Numeric.LinearAlgebra.Vector
import Numeric.LinearAlgebra.OrthoNormalBasis
import Graphics.Rendering.Haray.Ray

data Camera a = Camera
  { camCenter     :: !(Vec3 a)
  , camCorner     :: !(Vec3 a)
  , camAcross     :: !(Vec3 a)
  , camUp         :: !(Vec3 a)
  , camUVW        :: !(ONB a)
  , camLensRadius :: !a
  , camU0         :: !a
  , camU1         :: !a
  , camV0         :: !a
  , camV1         :: !a
  , camD          :: !a
  } deriving (Read, Show, Eq, Ord)

mkCamera :: Floating a => Vec3 a -> Vec3 a -> Vec3 a -> a -> a
         -> a -> a -> a -> a -> Camera a
mkCamera c gaze vup apeture left right bottom top distance =
  let lensRadius = apeture/2
      uvw = mkFromWV (vNegate gaze) vup
      corner = c <+> (left*>(onbU uvw))
                 <+> (bottom*>(onbV uvw))
                 <-> (distance*>(onbW uvw))
      across = (right - left)*>(onbU uvw)
      up = (top - bottom)*>(onbV uvw)
  in Camera { camCenter     = c
            , camCorner     = corner
            , camAcross     = across
            , camUp         = up
            , camUVW        = uvw
            , camLensRadius = lensRadius
            , camU0         = left
            , camU1         = right
            , camV0         = bottom
            , camV1         = top
            , camD          = distance
            }

getRay :: Floating a => Camera a -> a -> a -> Ray a
getRay cam a b =
  let origin = camCenter cam
      target = camCorner cam <+> (a*>(camAcross cam)) <+> (b*>(camUp cam))
  in Ray origin (unitVector (target <-> origin))
{-# SPECIALIZE INLINE getRay :: Camera Double -> Double -> Double -> Ray Double #-}
{-# SPECIALIZE INLINE getRay :: Camera Float  -> Float  -> Float  -> Ray Float  #-}
