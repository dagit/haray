module Graphics.Rendering.Haray.HitRecord where

import Numeric.LinearAlgebra.Vector
import Graphics.Rendering.Haray.Texture

data HitRecord a = HitRecord
  { hrT      :: !a
  , hrNormal :: !(Vec3 a)
  , hrUV     :: !(Vec2 a)
  , hrHitP   :: !(Vec3 a)
  , hrHitTex :: !(Texture a)
  }
