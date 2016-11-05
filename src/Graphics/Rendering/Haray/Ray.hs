module Graphics.Rendering.Haray.Ray where

import Numeric.LinearAlgebra.Vector

import Prelude hiding ((*>))

#ifdef USE_OPENCL
import Language.C.Syntax
import Language.C.Quote.OpenCL
#endif

data Ray a = Ray
  { rayOrigin    :: !(Vec3 a)
  , rayDirection :: !(Vec3 a)
  } deriving (Read, Show, Eq, Ord)

{-# INLINE pointAtParameter #-}
pointAtParameter :: Floating a => Ray a -> a -> Vec3 a
pointAtParameter (Ray o d) t = o <+> (t*>d)

#ifdef USE_OPENCL

rayDefinition :: [Definition]
rayDefinition = [cunit|
typedef struct Ray {
  float3 origin;
  float3 direction;
} Ray;

float3 pointAtParameter(const Ray * r, const float t)
{
  return r->origin + t*r->direction;
  // TODO: can it be written this way?
  // return mad(t,r->direction,r->origin);
}
|]

#endif
