module Graphics.Rendering.Haray.HitRecord where

import Control.DeepSeq
import Numeric.LinearAlgebra.Vector
import Graphics.Rendering.Haray.Texture

#ifdef USE_OPENCL
import Language.C.Syntax
import Language.C.Quote.OpenCL
#endif

data HitRecord a = HitRecord
  { hrT      :: !a
  , hrNormal :: !(Vec3 a)
  , hrUV     :: !(Vec2 a)
  , hrHitP   :: !(Vec3 a)
  , hrHitTex :: !(Texture a)
  }

instance NFData (HitRecord a) where
  rnf (HitRecord t normal uv hitp hittex) =
    t `seq` normal `seq` uv `seq` hitp `seq` hittex `seq` ()

#ifdef USE_OPENCL

hitRecordDefinition :: [Definition]
hitRecordDefinition = [cunit|
typedef struct HitRecord {
  float  t;
  float3 normal;
  float3 color;
} HitRecord;
|] 


#endif
