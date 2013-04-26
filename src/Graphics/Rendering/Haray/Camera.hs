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

translateCamera :: Floating a => Camera a -> Vec3 a -> Camera a
translateCamera c v = c { camCenter = camCenter c <+> v }

getRay :: Floating a => Camera a -> a -> a -> Ray a
getRay cam a b =
  let origin = camCenter cam
      target = camCorner cam <+> (a*>(camAcross cam)) <+> (b*>(camUp cam))
  in Ray origin (unitVector (target <-> origin))
{-# SPECIALIZE INLINE getRay :: Camera Double -> Double -> Double -> Ray Double #-}
{-# SPECIALIZE INLINE getRay :: Camera Float  -> Float  -> Float  -> Ray Float  #-}

getFishEyeRay :: Floating a => Camera a -> a -> a -> a -> a -> Ray a
getFishEyeRay cam xp yp hres vres =
  let origin = camCenter cam
      corner = camCorner cam
      xn     = 2*(xp / hres) - 1
      yn     = 2*(yp / vres) - 1
      uvw    = camUVW cam
      target = (((xn / r)*sfov) *> onbU uvw) <+> ((sfov * (yn / r)) *> onbV uvw) <-> (cfov *> onbW uvw)
      r      = sqrt (xn*xn + yn*yn)
      cfov   = cos (r*fov)
      sfov   = sin (r*fov)
      fov    = 45 / 180 * pi
  in Ray origin (unitVector (target <-> origin))

-- | f(r) = k0 + k1*r^2 + k2*r^4 + k3*r^6
-- where r is the radius. This function tells you how much to distort
-- the r, so (r,theta) |--> (f(r)*r, theta)
getBarrelRay :: Floating a => Camera a -> Vec4 a -> a -> a -> a -> a -> Ray a
getBarrelRay cam (Vec4 k0 k1 k2 k3) xp yp hres vres =
  let origin = camCenter cam
      corner = camCorner cam
      xn     = 2*(xp / hres) - 1
      yn     = 2*(yp / vres) - 1
      uvw    = camUVW cam
      -- theta  = atan (yn/xn)
      target = ((k0 + k1*rSq + k2*rSq*rSq + k3*rSq*rSq*rSq) *> (Vec3 xn yn 0)) <-> (cfov *> onbW uvw)
      rSq    = xn*xn + yn*yn
      cfov   = cos (sqrt rSq*fov)
      fov    = 90 / 180 * pi
  in Ray origin (unitVector (target <-> origin))
