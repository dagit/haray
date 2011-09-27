module Data.OrthoNormalBasis where

import Data.VectorSpace

epsilon :: (Ord a, Floating a) => a
epsilon = 0.01

data ONB a = ONB
  { onbU :: Vec3 a
  , onbV :: Vec3 a
  , onbW :: Vec3 a
  } deriving (Read, Show, Eq, Ord)

mkFromU :: (Ord a, Floating a) => Vec3 a -> ONB a
mkFromU u' = ONB u v w
  where
  n = Vec3 1 0 0
  m = Vec3 0 1 0
  u = unitVector u'
  v = if len (u <%> n) < epsilon
        then u <%> m
        else u <%> n
  w = u <%> v

mkFromV :: (Ord a, Floating a) => Vec3 a -> ONB a
mkFromV v' = ONB u v w
  where
  n = Vec3 1 0 0
  m = Vec3 0 1 0
  v = unitVector v'
  u = if lenSquared (v <%> n) < epsilon
        then v <%> m
        else v <%> n
  w = u <%> v

mkFromW :: (Ord a, Floating a) => Vec3 a -> ONB a
mkFromW w' = ONB u v w
  where
  n = Vec3 1 0 0
  m = Vec3 0 1 0
  w = unitVector w'
  u = if len (w <%> n) < epsilon
        then w <%> m
        else w <%> n
  v = w <%> u

mkFromUV :: Floating a => Vec3 a -> Vec3 a -> ONB a
mkFromUV u' v' = ONB u v w
  where
  u = unitVector u'
  w = unitVector (u' <%> v')
  v = w <%> u

mkFromVU :: Floating a => Vec3 a -> Vec3 a -> ONB a
mkFromVU v' u' = ONB u v w
  where
  v = unitVector v'
  w = unitVector (u' <%> v')
  u = v <%> w

mkFromUW :: Floating a => Vec3 a -> Vec3 a -> ONB a
mkFromUW u' w' = ONB u v w
  where
  u = unitVector u'
  v = unitVector (w' <%> u')
  w = u <%> v

mkFromWU :: Floating a => Vec3 a -> Vec3 a -> ONB a
mkFromWU w' u' = ONB u v w
  where
  w = unitVector w'
  v = unitVector (w' <%> u')
  u = v <%> w

mkFromVW :: Floating a => Vec3 a -> Vec3 a -> ONB a
mkFromVW v' w' = ONB u v w
  where
  v = unitVector v'
  u = unitVector (v' <%> w')
  w = u <%> v

mkFromWV :: Floating a => Vec3 a -> Vec3 a -> ONB a
mkFromWV w' v' = ONB u v w
  where
  w = unitVector w'
  u = unitVector (v' <%> w')
  v = w <%> u 
