module Graphics.Rendering.Haray.Ray where

import Numeric.LinearAlgebra.Vector

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

rayStruct :: Definition
rayStruct = [cedecl|
typedef struct Ray {
  float3 origin;
  float3 direction;
} Ray;
|]

rayPointAtParameter :: Definition
rayPointAtParameter = [cedecl|
float3 pointAtParameter(struct Ray * r, float t)
{
  return r->origin + t*r->direction;
}
|]

#endif
