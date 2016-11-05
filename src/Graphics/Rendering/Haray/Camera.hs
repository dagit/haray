{-# LANGUAGE BangPatterns #-}
module Graphics.Rendering.Haray.Camera where

import Numeric.LinearAlgebra.Vector
import Numeric.LinearAlgebra.OrthoNormalBasis
import Graphics.Rendering.Haray.Ray

import Prelude hiding ((*>))

#ifdef USE_OPENCL
import Language.C.Syntax
import Language.C.Quote.OpenCL
#endif

data LensType a
  = Pinhole
  | FishEye
  -- TODO: get the distortion from the HMDInfo during parsing
  | Barrel !(Vec4 a) -- ^ Distortion coefficients
  deriving (Read, Show, Eq, Ord)

data Camera a = Camera
  { camCenter     :: !(Vec3 a)       -- ^ center of the camera in world coordinates
  , camUVW        :: !(ONB a)        -- ^ local coordinate space of the camera,
                                     -- we construct this from gaze and up vector
  , camD          :: !a              -- ^ distance from camera center to film plane
  , camNX         :: !Int            -- ^ horizontal pixel count
  , camNY         :: !Int            -- ^ vertical pixel count
  , camFov        :: !a              -- ^ vertical field of view
  , camRay        :: a -> a -> Maybe (Ray a) -- ^ takes an (x,y) pixel coordinate and generates a ray, Nothing
                                             -- Means the ray was clipped
  }

mkCamera :: (Ord a, Floating a) => LensType a
         -> Vec3 a -> Vec3 a -> Vec3 a -> a -> a
         -> Int -> Int -> Camera a
mkCamera lens center gaze vup distance fov nx ny
  = case lens of
    Pinhole           -> cam perspectiveRay
    FishEye           -> cam fishEyeRay
    Barrel distortion -> cam (flip barrelRay distortion)
  where
  -- Construct a camera from a ray function and make
  -- sure the ray function applies to the same camera.
  cam f = let c = mkCamera' (f c) in c
  mkCamera' ray = Camera
    { camCenter = center
    , camUVW    = uvw
    , camFov    = fov
    , camD      = distance
    , camNX     = nx
    , camNY     = ny
    , camRay    = ray
    }
  uvw = mkFromWV (vNegate gaze) vup
-- Yes, these really do make a difference. Without them GHC seems to use
-- unspecialized versions of camRay.
{-# SPECIALIZE INLINE mkCamera :: LensType Double
                               -> Vec3 Double -> Vec3 Double -> Vec3 Double
                               -> Double -> Double -> Int -> Int -> Camera Double #-}
{-# SPECIALIZE INLINE mkCamera :: LensType Float
                               -> Vec3 Float -> Vec3 Float -> Vec3 Float
                               -> Float -> Float -> Int -> Int -> Camera Float #-}

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
perspectiveRay :: Floating a
               => Camera a -- ^ the camera, defines UV plane and position in space
               -> a        -- ^ xp, x in pixel coordinates
               -> a        -- ^ yp, y in pixel coordinates
               -> Maybe (Ray a)
perspectiveRay cam xp yp =
  let origin = camCenter cam
      hres   = fromIntegral (camNX cam)
      vres   = fromIntegral (camNY cam)
      xn     = 2*(xp / hres) - 1
      yn     = 2*(yp / vres) - 1
      uvw    = camUVW cam
      -- TODO: incorporate fov
      target = (xn *> onbU uvw) <+> (yn*> onbV uvw) <-> (camD cam *> onbW uvw)
  in Just (Ray origin (unitVector target))
{-# SPECIALIZE INLINE perspectiveRay :: Camera Double -> Double -> Double -> Maybe (Ray Double) #-}
{-# SPECIALIZE INLINE perspectiveRay :: Camera Float  -> Float  -> Float  -> Maybe (Ray Float)  #-}

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
fishEyeRay :: (Ord a, Floating a)
           => Camera a -- ^ the camera, defines the UV plane and the location in space
           -> a        -- ^ xp, x in pixel coordinates
           -> a        -- ^ yp, y in pixel coordinates
           -> Maybe (Ray a)
fishEyeRay cam xp yp =
  let origin = camCenter cam
      hres   = fromIntegral (camNX cam)
      vres   = fromIntegral (camNY cam)
      xn     = 2*(xp / hres) - 1
      yn     = 2*(yp / vres) - 1
      uvw    = camUVW cam
      target = (((xn / r)*sfov) *> onbU uvw) <+> ((sfov * (yn / r)) *> onbV uvw) <-> (cfov *> onbW uvw)
      !r     = sqrt (xn*xn + yn*yn)
      cfov   = cos (r*fov)
      sfov   = sin (r*fov)
      fov    = camFov cam / 180 * pi
  in if (r <= 1)
       then Just (Ray origin (unitVector target))
       else Nothing
{-# SPECIALIZE INLINE fishEyeRay :: Camera Double -> Double -> Double -> Maybe (Ray Double) #-}
{-# SPECIALIZE INLINE fishEyeRay :: Camera Float  -> Float  -> Float  -> Maybe (Ray Float)  #-}

{- |
Applies a distortion based on a polynomial. The coefficients of the polynomial are passed as
the second parameter.  The distortion preserves angle but distorts distance from the center:

> f(r) = k0 + k1*r^2 + k2*r^4 + k3*r^6
> (r,theta) maps to (f(r)*r, theta)

where r is the radius of the point on the UV image plane.
-}
barrelRay :: (Ord a, Floating a)
          => Camera a -- ^ the camera, defines UV plane and location in space
          -> Vec4 a   -- ^ Distortion coefficients, passed as 'Vec3 k0 k1 k2 k3'
          -> a        -- ^ xp, x in pixel coordinates
          -> a        -- ^ yp, y in pixel coordinates
          -> Maybe (Ray a)
barrelRay cam (Vec4 k0 k1 k2 k3) xp yp =
  let origin = camCenter cam
      hres   = fromIntegral (camNX cam)
      vres   = fromIntegral (camNY cam)
      xn     = 2*(xp / hres) - 1
      yn     = 2*(yp / vres) - 1
      uvw    = camUVW cam
      !scale = k0 + k1*rSq + k2*rSq*rSq + k3*rSq*rSq*rSq
      xrad   = xn*scale
      yrad   = yn*scale
      target = Vec3 xrad yrad 0 <-> (cfov *> onbW uvw)
      rSq    = xn*xn + yn*yn
      cfov   = cos (sqrt rSq*fov)
      fov    = camFov cam / 180 * pi
  in if (-1.25 <= xrad && xrad <= 1.25 && -1.25 <= yrad && yrad <= 1.25)
       then Just (Ray origin (unitVector target))
       else Nothing
{-# SPECIALIZE INLINE barrelRay :: Camera Double -> Vec4 Double -> Double -> Double -> Maybe (Ray Double) #-}
{-# SPECIALIZE INLINE barrelRay :: Camera Float  -> Vec4 Float  -> Float  -> Float  -> Maybe (Ray Float)  #-}

#ifdef USE_OPENCL
cameraDefinition :: [Definition]
cameraDefinition = [cunit|
typedef struct ONB {
  float3 u;
  float3 v;
  float3 w;
} ONB;

ONB mkFromWV(float3 w, float3 v)
{
  ONB uvw;
  uvw.v = normalize(v);
  uvw.u = normalize(cross(v,w));
  uvw.w = cross(uvw.u, uvw.v);
  return uvw;
}

typedef struct Camera {
  float3 center;
  ONB    uvw;
  float  d;
  int    nx;
  int    ny;
  float  fov;
  // We have to put the camRay below as we can't
  // have function pointers in structs
} Camera;

Camera mkCamera(float3 center, float3 gaze, float3 vup
               ,float distance, float fov, int nx, int ny)
{
  Camera cam;
  cam.center = center;
  cam.d      = distance;
  cam.nx     = nx;
  cam.ny     = ny;
  cam.fov    = fov;
  cam.uvw    = mkFromWV(- gaze, vup);
  return cam;
}

// Generates a perspectiveRay with an option for clipping the ray
// although, in the case of the normal camera the ray never gets
// clipped.
bool
camRay(const Camera cam, const float xp, const float yp, struct Ray * ray){
  float xn = 2*(xp / cam.nx) - 1;
  float yn = 2*(yp / cam.ny) - 1;

  ray->origin    = cam.center;
  ray->direction = xn * cam.uvw.u + yn * cam.uvw.v - cam.d * cam.uvw.w;
  return true;
}
|]
#endif
