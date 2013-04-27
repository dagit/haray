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
{-# SPECIALIZE INLINE translateCamera :: Camera Double -> Vec3 Double -> Camera Double #-}
{-# SPECIALIZE INLINE translateCamera :: Camera Float  -> Vec3 Float  -> Camera Float  #-}

{- | Computes a standard ray from the camera. The camera is assumed to behave like
a pinhole camera (no distortion or lens effects).

The pixel coordinates are mapped into the UV film plane of the camera. Then a
ray computed that starts from the origin of the camera and passes through the
computed point on the UV plane. This gives a perspective projection.

-}
getPerspectiveRay :: Floating a
       => Camera a -- ^ the camera, defines UV plane and position in space
       -> a        -- ^ xp, x in pixel coordinates
       -> a        -- ^ yp, y in pixel coordinates
       -> a        -- ^ hres, horizontal resolution as a count of pixels (and converted to Floating)
       -> a        -- ^ vres, vertical resolution as a count of pixels (and converted to Floating)
       -> Ray a
getPerspectiveRay cam xp yp hres vres =
  let origin = camCenter cam
      nx     = 2*(xp / hres) - 1
      ny     = 2*(yp / vres) - 1
      uvw    = camUVW cam
      target = (nx *> onbU uvw) <+> (ny*> onbV uvw) <-> (camD cam *> onbW uvw)
  in Ray origin (unitVector target)
{-# SPECIALIZE INLINE getPerspectiveRay :: Camera Double -> Double -> Double -> Double -> Double -> Ray Double #-}
{-# SPECIALIZE INLINE getPerspectiveRay :: Camera Float  -> Float  -> Float  -> Float  -> Float  -> Ray Float  #-}

{- |

Fish eye lenses distort the image such that (x,y) in [-1, 1] maps onto the UV
image plane as:

> (sin upsilon * cos theta)u + (sin upsilon * sin theta)v - (cos upsilon)w
> where
> upsilon = r * fov / 2
> cos theta = x/r
> sin theta = y/r

In other words, this transformation requires a change to polar coordinates
and then a change to spherical coordinates.

xp and yp are assumed to be in pixel coordinates. That is, ranging from
[0,hres) and [0,vres) respectively. Thus, we must map them into [-1,1] before
we can even begin.

For more details about this transformation see these slides:

> http://web.cs.wpi.edu/~emmanuel/courses/cs563/S10/talks/wk4_p2_joe_nonlinear_projections.pdf

-}
getFishEyeRay :: Floating a
              => Camera a -- ^ the camera, defines the UV plane and the location in space
              -> a        -- ^ xp, x in pixel coordinates
              -> a        -- ^ yp, y in pixel coordinates
              -> a        -- ^ hres, horizontal resolution as a count of pixels (and converted to Floating)
              -> a        -- ^ vres, vertical resolution as a count of pixels (and converted to Floating)
              -> Ray a
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
  -- in Ray origin (unitVector (target <-> origin))
  in Ray origin (unitVector target)
{-# SPECIALIZE INLINE getFishEyeRay :: Camera Double -> Double -> Double -> Double -> Double -> Ray Double #-}
{-# SPECIALIZE INLINE getFishEyeRay :: Camera Float  -> Float  -> Float  -> Float  -> Float  -> Ray Float  #-}

{- |
Applies a distortion based on a polynomial. The coefficients of the polynomial are passed as
the second parameter.  The distortion preserves angle but distorts distance from the center:

> f(r) = k0 + k1*r^2 + k2*r^4 + k3*r^6
> (r,theta) maps to (f(r)*r, theta)

where r is the radius of the point on the UV image plane.
-}
getBarrelRay :: Floating a
             => Camera a -- ^ the camera, defines UV plane and location in space
             -> Vec4 a   -- ^ Distortion coefficients, passed as 'Vec3 k0 k1 k2 k3'
             -> a        -- ^ xp, x in pixel coordinates
             -> a        -- ^ yp, y in pixel coordinates
             -> a        -- ^ hres, horizontal resolution as a count of pixels (and converted to Floating)
             -> a        -- ^ vres, horizontal resolution as a count of pixels (and converted to Floating)
             -> Ray a
getBarrelRay cam (Vec4 k0 k1 k2 k3) xp yp hres vres =
  let origin = camCenter cam
      corner = camCorner cam
      xn     = 2*(xp / hres) - 1
      yn     = 2*(yp / vres) - 1
      uvw    = camUVW cam
      target = ((k0 + k1*rSq + k2*rSq*rSq + k3*rSq*rSq*rSq) *> (Vec3 xn yn 0)) <-> (cfov *> onbW uvw)
      rSq    = xn*xn + yn*yn
      cfov   = cos (sqrt rSq*fov)
      fov    = 90 / 180 * pi
  -- in Ray origin (unitVector (target <-> origin))
  in Ray origin (unitVector target)
{-# SPECIALIZE INLINE getBarrelRay :: Camera Double -> Vec4 Double -> Double -> Double -> Double -> Double -> Ray Double #-}
{-# SPECIALIZE INLINE getBarrelRay :: Camera Float  -> Vec4 Float  -> Float  -> Float  -> Float  -> Float  -> Ray Float  #-}
