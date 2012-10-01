module Graphics.Rendering.Haray.Shape where

import Graphics.Rendering.Haray.Ray
import Graphics.Rendering.Haray.HitRecord
import Graphics.Rendering.Haray.Texture
import Numeric.LinearAlgebra.Vector

data Shape a = Shape
  { shapeHit       :: Ray a -> a -> a -> a -> Maybe (HitRecord a)
  , shapeShadowHit :: Ray a -> a -> a -> a -> Bool
  }

data TriangleData a = TriangleData
  { tdP0  :: !(Vec3 a)
  , tdP1  :: !(Vec3 a)
  , tdP2  :: !(Vec3 a)
  , tdTex :: Texture a
  }

{-# INLINE mkTriangle #-}
mkTriangle :: (Ord a, Floating a) => TriangleData a -> Shape a
mkTriangle td = Shape
  { shapeHit = \r tmin tmax _time -> {-# SCC "triangleShapeHit" #-}
    let p0 = tdP0 td
        p1 = tdP1 td
        p2 = tdP2 td
        a = vElement p0               0 - vElement p1 0 
        b = vElement p0               1 - vElement p1 1 
        c = vElement p0               2 - vElement p1 2
        d = vElement p0               0 - vElement p2 0
        e = vElement p0               1 - vElement p2 1
        f = vElement p0               2 - vElement p2 2
        g = vElement (rayDirection r) 0
        h = vElement (rayDirection r) 1
        i = vElement (rayDirection r) 2
        j = vElement p0               0 - vElement (rayOrigin r) 0
        k = vElement p0               1 - vElement (rayOrigin r) 1
        l = vElement p0               2 - vElement (rayOrigin r) 2
        eihf = e*i-h*f
        gfdi = g*f-d*i
        dheg = d*h-e*g
        denom = a*eihf + b*gfdi + c*dheg
        beta = (j*eihf + k*gfdi + l*dheg) / denom
        akjb = a*k - j*b
        jcal = j*c - a*l
        blkc = b*l - k*c
        gamma = (i*akjb + h*jcal + g*blkc)/denom
        tval = -(f*akjb + e*jcal + d*blkc)/denom
    in
    if beta <= 0 || beta >= 1
      then Nothing
      else if gamma <= 0 || beta + gamma >= 1
             then Nothing
             else if tval >= tmin && tval <= tmax
                    then Just (HitRecord
                      { hrT      = tval
                      , hrNormal = unitVector ((p1 <-> p0) <%> (p2 <-> p0))
                      , hrHitTex = (tdTex td)
                      , hrHitP   = (tval *> rayDirection r) <+> rayOrigin r
                      , hrUV     = Vec2 0 0 -- TODO: implement me
                      })
                    else Nothing
  , shapeShadowHit = \r tmin tmax _time ->
    let p0 = tdP0 td
        p1 = tdP1 td
        p2 = tdP2 td
        a = vElement p0               0 - vElement p1 0
        b = vElement p0               1 - vElement p1 1
        c = vElement p0               2 - vElement p1 2
        d = vElement p0               0 - vElement p2 0
        e = vElement p0               1 - vElement p2 1
        f = vElement p0               2 - vElement p2 2
        g = vElement (rayDirection r) 0
        h = vElement (rayDirection r) 1
        i = vElement (rayDirection r) 2
        j = vElement p0               0 - vElement (rayOrigin r) 0
        k = vElement p0               1 - vElement (rayOrigin r) 1
        l = vElement p0               2 - vElement (rayOrigin r) 2
        eihf = e*i-h*f
        gfdi = g*f-d*i
        dheg = d*h-e*g
        denom = a*eihf + b*gfdi + c*dheg
        beta = (j*eihf + k*gfdi + l*dheg)/denom
        akjb = a*k - j*b
        jcal = j*c - a*l
        blkc = b*l - k*c
        gamma = (i*akjb + h*jcal + g*blkc)/denom
        tval = -(f*akjb + e*jcal + d*blkc)/denom
    in
    if beta <= 0 || beta >= 1
      then False
      else if gamma <= 0 || beta + gamma >= 1
             then False
             else tval >= tmin && tval <= tmax
  }

data SphereData a = SphereData
  { sphereCenter :: !(Vec3 a)
  , sphereRadius :: !a
  , sphereTex    :: Texture a
  }

{-# INLINE mkSphere #-}
mkSphere :: (Ord a, Floating a) => SphereData a -> Shape a
mkSphere sd = Shape
  { shapeHit = \r tmin tmax _time -> {-# SCC "sphereShapeHit" #-}
    let temp = rayOrigin r <-> sphereCenter sd
        a = rayDirection r <.> rayDirection r
        b = 2*(rayDirection r <.> temp)
        c = (temp <.> temp) - (sphereRadius sd * sphereRadius sd)
        discriminant' = b*b - 4*a*c
    in
    if discriminant' > 0
      then let discriminant = sqrt discriminant'
               t = ((-b) - discriminant)/(2*a)
               t'= ((-b) + discriminant)/(2*a)
               getHit at = Just (HitRecord
                 {hrT = at
                 ,hrNormal = unitVector (rayOrigin r <+> (at *> rayDirection r)
                                        <-> sphereCenter sd)
                 ,hrHitTex = sphereTex sd
                 ,hrHitP   = rayOrigin r <+> (at *> rayDirection r)
                 ,hrUV     = Vec2 0 0 -- TODO: implement me
                 })
      in
      if t < tmin
        then if t' < tmin || t' > tmax
               then Nothing
               else getHit t'
        else getHit t
      else Nothing
  , shapeShadowHit = \r tmin tmax _time ->
    let temp = rayOrigin r <-> sphereCenter sd
        a = rayDirection r <.> rayDirection r
        b = 2*(rayDirection r <.> temp)
        c = (temp <.> temp) - (sphereRadius sd * sphereRadius sd)
        discriminant' = b*b - 4*a*c
    in
    if discriminant' > 0
      then let discriminant = sqrt discriminant'
               t  = ((-b) - discriminant)/(2*a)
               t' = ((-b) + discriminant)/(2*a)
      in
      if t < tmin
        then if t' < tmin || t' > tmax
               then False
               else True
        else True
      else False
  }

data PlaneData a = PlaneData
  { pdCenter :: !(Vec3 a)
  , pdNormal :: !(Vec3 a)
  , pdTex    :: Texture a
  }

{-# INLINE mkPlane #-}
mkPlane :: (Ord a, Floating a) => PlaneData a -> Shape a
mkPlane pd = Shape
  { shapeHit = \r tmin tmax _time ->
    let n   = pdNormal pd
        p0  = pdCenter pd
        l   = rayDirection r
        l0  = rayOrigin r
        eps = 0.01
        numer = (p0 <-> l0) <.> n
        denom = l <.> n
        tval  = numer / denom
    in
    if abs denom < eps
      then Nothing
      else if tmin <= tval && tval <= tmax
             then Just $ HitRecord
               { hrT      = numer / denom
               , hrNormal = n
               , hrHitTex = pdTex pd
               , hrHitP   = (tval *> l) <+> l0
               , hrUV     = Vec2 0 0 -- TODO: implement me
               }
             else Nothing
  , shapeShadowHit = \r tmin tmax _time ->
    let n   = pdNormal pd
        p0  = pdCenter pd
        l   = rayDirection r
        l0  = rayOrigin r
        eps = 0.01
        numer = (p0 <-> l0) <.> n
        denom = l <.> n
        tval = numer / denom
    in
    if abs denom < eps
      then False
      else tmin <= tval && tval <= tmax
  }
