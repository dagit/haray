{-# LANGUAGE TypeFamilies #-}
module Data.VectorSpace where

import Data.List ( foldl1' )

data Vec4 a = Vec4 !a !a !a !a
  deriving (Read, Show, Eq, Ord)
data Vec3 a = Vec3 !a !a !a
  deriving (Read, Show, Eq, Ord)
data Vec2 a = Vec2 !a !a
  deriving (Read, Show, Eq, Ord)

data Mat44 a = Mat44
  { m00 :: !a, m01 :: !a, m02 :: !a, m03 :: !a
  , m10 :: !a, m11 :: !a, m12 :: !a, m13 :: !a
  , m20 :: !a, m21 :: !a, m22 :: !a, m23 :: !a
  , m30 :: !a, m31 :: !a, m32 :: !a, m33 :: !a
  }
  deriving (Read, Show, Eq, Ord)

class Vector v where
  type Scalar v :: *
  vDim     ::                          v -> Int
  vElement :: (k ~ Scalar v)        => Int -> v -> k
  vIndexOf :: (k ~ Scalar v, Ord k) => (k -> k -> Bool) -> v -> Int
  vZip     :: (k ~ Scalar v)        => (k -> k -> k) -> v -> v -> v
  vMap     :: (k ~ Scalar v)        => (k -> k) -> v -> v
  vIdxMap  :: (k ~ Scalar v)        => (Int -> k) -> v -> v
  vFold    :: (k ~ Scalar v)        => (k -> k -> k) -> v -> k

class Matrix m where
  type Element m :: *
  mDim     ::                           m -> Int
  mElement :: (k ~ Element m)        => Int -> Int -> m -> k
  mIndexOf :: (k ~ Element m, Ord k) => (k -> k -> Bool) -> m -> (Int, Int)
  mZip     :: (k ~ Element m)        => (k -> k -> k) -> m -> m -> m
  mMap     :: (k ~ Element m)        => (k -> k) -> m -> m
  mIdxMap  :: (k ~ Element m)        => (Int -> Int -> k) -> m -> m
  mFold    :: (k ~ Element m)        => (k -> k -> k) -> m -> k

instance Floating a => Matrix (Mat44 a) where
  type Element (Mat44 a) = a
  {-# INLINE mDim #-}
  mDim _ = 4
  {-# INLINE mElement #-}
  mElement 0 0 = m00
  mElement 0 1 = m01
  mElement 0 2 = m02
  mElement 0 3 = m03
  mElement 1 0 = m10
  mElement 1 1 = m11
  mElement 1 2 = m12
  mElement 1 3 = m13
  mElement 2 0 = m20
  mElement 2 1 = m21
  mElement 2 2 = m22
  mElement 2 3 = m23
  mElement 3 0 = m30
  mElement 3 1 = m31
  mElement 3 2 = m32
  mElement 3 3 = m33
  mElement i j = \_ -> error ("Index " ++ show i ++ ", " ++ show j ++ ": out of range, must be 0,0 to 3,3")
  {-# INLINE mMap #-}
  mMap f m = Mat44 (f (m00 m)) (f (m01 m)) (f (m02 m)) (f (m03 m))
                   (f (m10 m)) (f (m11 m)) (f (m12 m)) (f (m13 m))
                   (f (m20 m)) (f (m21 m)) (f (m22 m)) (f (m23 m))
                   (f (m30 m)) (f (m31 m)) (f (m32 m)) (f (m33 m))
  {-# INLINE mIdxMap #-}
  mIdxMap f m = Mat44 (f 0 0) (f 0 1) (f 0 2) (f 0 3)
                      (f 1 0) (f 1 1) (f 1 2) (f 1 3)
                      (f 2 0) (f 2 1) (f 2 2) (f 2 3)
                      (f 3 0) (f 3 1) (f 3 2) (f 3 3)
  {-# INLINE mZip #-}
  mZip f m n = Mat44 (f (m00 m) (m00 n)) (f (m01 m) (m01 n)) (f (m02 m) (m02 n)) (f (m03 m) (m03 n))
                     (f (m10 m) (m10 n)) (f (m11 m) (m11 n)) (f (m12 m) (m12 n)) (f (m13 m) (m13 n))
                     (f (m20 m) (m20 n)) (f (m21 m) (m21 n)) (f (m22 m) (m22 n)) (f (m23 m) (m23 n))
                     (f (m30 m) (m30 n)) (f (m31 m) (m31 n)) (f (m32 m) (m32 n)) (f (m33 m) (m33 n))
  {-# INLINE mFold #-}
  mFold f m = foldl1' f [ mElement i j m | i <- [ 0 .. 3 ], j <- [ 0 .. 3 ] ]
  {-# INLINE mIndexOf #-}
  mIndexOf p m = fst (foldl1' p' [ ((i,j), mElement i j m) | j <- [ 3, 2 .. 0 ], i <- [ 3, 2 .. 0 ] ])
    where
    p' x@(_, a) y@(_, a') | a `p` a'  = x
                          | otherwise = y

instance Floating a => Vector (Vec4 a) where
  type Scalar (Vec4 a) = a
  {-# INLINE vDim #-}
  vDim _ = 4
  {-# INLINE vElement #-}
  vElement 0 (Vec4 x _ _ _) = x
  vElement 1 (Vec4 _ y _ _) = y
  vElement 2 (Vec4 _ _ z _) = z
  vElement 3 (Vec4 _ _ _ w) = w
  vElement i _ = error ("Index " ++ show i ++ ": out of range, must be 0 to 3")
  {-# INLINE vMap #-}
  vMap f (Vec4 x y z w) = Vec4 (f x) (f y) (f z) (f w)
  {-# INLINE vIdxMap #-}
  vIdxMap f (Vec4 x y z w) = Vec4 (f 0) (f 1) (f 2) (f 3)
  {-# INLINE vZip #-}
  vZip f (Vec4 x1 y1 z1 w1) (Vec4 x2 y2 z2 w2) = Vec4 (f x1 x2) (f y1 y2) (f z1 z2) (f w1 w2)
  {-# INLINE vFold #-}
  vFold f (Vec4 x y z w) = f (f (f x y) z) w
  {-# INLINE vIndexOf #-}
  vIndexOf p (Vec4 x y z w) | w `p` x && w `p` y && w `p` z = 3
                            | z `p` x && z `p` y && z `p` w = 2
                            | y `p` x && y `p` z && y `p` w = 1
                            | otherwise = 0

instance Floating a => Vector (Vec3 a) where
  type Scalar (Vec3 a) = a
  {-# INLINE vDim #-}
  vDim _ = 3
  {-# INLINE vElement #-}
  vElement 0 (Vec3 x _ _) = x
  vElement 1 (Vec3 _ y _) = y
  vElement 2 (Vec3 _ _ z) = z
  vElement i _ = error ("Index " ++ show i ++ ": out of range, must be 0 to 2")
  {-# INLINE vMap #-}
  vMap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)
  {-# INLINE vIdxMap #-}
  vIdxMap f (Vec3 x y z) = Vec3 (f 0) (f 1) (f 2)
  {-# INLINE vZip #-}
  vZip f (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (f x1 x2) (f y1 y2) (f z1 z2)
  {-# INLINE vFold #-}
  vFold f (Vec3 x y z) = f (f x y) z
  {-# INLINE vIndexOf #-}
  vIndexOf p (Vec3 x y z) | z `p` x && z `p` y = 2
                          | y `p` x && y `p` z = 1
                          | otherwise = 0

instance Floating a => Vector (Vec2 a) where
  type Scalar (Vec2 a) = a
  {-# INLINE vDim #-}
  vDim _ = 2
  {-# INLINE vElement #-}
  vElement 0 (Vec2 x _) = x
  vElement 1 (Vec2 _ y) = y
  vElement i _ = error ("Index " ++ show i ++ ": out of range, must be 0 or 1")
  {-# INLINE vMap #-}
  vMap f (Vec2 x y) = Vec2 (f x) (f y)
  {-# INLINE vIdxMap #-}
  vIdxMap f (Vec2 x y) = Vec2 (f 0) (f 1)
  {-# INLINE vZip #-}
  vZip f (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (f x1 x2) (f y1 y2)
  {-# INLINE vFold #-}
  vFold f (Vec2 x y) = f x y
  {-# INLINE vIndexOf #-}
  vIndexOf p (Vec2 x y) | y `p` x   = 1
                        | otherwise = 0

{-# INLINE vNegate #-}
vNegate :: (k ~ Scalar v, Num k, Vector v) => v -> v
vNegate = vMap negate
{-# INLINE (<+>) #-}
(<+>) :: (k ~ Scalar v, Num k, Vector v) => v -> v -> v
(<+>) = vZip (+)
{-# INLINE (<*>) #-}
(<*>) :: (k ~ Scalar v, Num k, Vector v) => v -> v -> v
(<*>) = vZip (*)
{-# INLINE (<->) #-}
(<->) :: (k ~ Scalar v, Num k, Vector v) => v -> v -> v
(<->) = vZip (-)
{-# INLINE (</>) #-}
(</>) :: (k ~ Scalar v, Fractional k, Vector v) => v -> v -> v
(</>) = vZip (/)
{-# INLINE (<.>) #-}
(<.>) :: (k ~ Scalar v, Num k, Vector v) => v -> v -> k -- dot product
v1 <.> v2 = vFold (+) (v1 <*> v2)
{-# INLINE (*>) #-}
(*>) :: (k ~ Scalar v, Num k, Vector v) => k -> v -> v
k *> v = vMap (k*) v
{-# INLINE (</) #-}
(</) :: (k ~ Scalar v, Fractional k, Vector v) => v -> k -> v
v </ k = vMap (/k) v
{-# INLINE len #-}
len :: (k ~ Scalar v, Floating k, Vector v) => v -> k
len v = sqrt $ lenSquared v
{-# INLINE lenSquared #-}
lenSquared :: (k ~ Scalar v, Num k, Vector v) => v -> k
lenSquared v = v <.> v

(.+.) :: (k ~ Element m, Num k, Matrix m) => m -> m -> m
(.+.) = mZip (+)
(.-.) :: (k ~ Element m, Num k, Matrix m) => m -> m -> m
(.-.) = mZip (-)
(.*.) :: (k ~ Element m, Num k, Matrix m) => m -> m -> m
m .*. n = flip mIdxMap m $ \i j -> sum [ mElement i k m * mElement k j n | k <- [ 0 .. 3 ] ]
(.*>) :: (k ~ Element m, k ~ Scalar v, Num k, Matrix m, Vector v)
      => m -> v -> v
m .*> v | vDim v == mDim m = flip vIdxMap v $ \k -> sum [ mElement i k m * vElement k v | i <- [ 0 .. mDim m ] ]
        | otherwise        = error "Dimensions do not match"
(*.) :: (k ~ Element m, Num k, Matrix m) => m -> k -> m
m *. k = mMap (k*) m

det3 :: Num a
     => a -> a -> a
     -> a -> a -> a
     -> a -> a -> a
     -> a
det3 a b c d e f g h i = a*e*i + d*h*c + g*b*f - g*e*c - d*b*i - a*h*f

determinant :: Num a => Mat44 a -> a
determinant m = m00 m * det3 (m11 m) (m12 m) (m13 m)
                             (m21 m) (m22 m) (m23 m)
                             (m31 m) (m32 m) (m33 m)
              - m01 m * det3 (m10 m) (m12 m) (m13 m)
                             (m20 m) (m22 m) (m23 m)
                             (m30 m) (m32 m) (m33 m)
              + m02 m * det3 (m10 m) (m11 m) (m13 m)
                             (m20 m) (m21 m) (m23 m)
                             (m30 m) (m31 m) (m33 m)
              - m03 m * det3 (m10 m) (m11 m) (m12 m)
                             (m20 m) (m21 m) (m22 m)
                             (m30 m) (m31 m) (m32 m)

invert :: Fractional a => Mat44 a -> Mat44 a
invert m = m'
  where
  det = determinant m
  m'  = Mat44 (det3 (m11 m) (m12 m) (m13 m)
                    (m21 m) (m22 m) (m23 m)
                    (m31 m) (m32 m) (m33 m) / det)
            (-(det3 (m01 m) (m02 m) (m03 m)
                    (m21 m) (m22 m) (m23 m)
                    (m31 m) (m32 m) (m33 m) / det))
              (det3 (m01 m) (m02 m) (m03 m)
                    (m11 m) (m12 m) (m13 m)
                    (m31 m) (m32 m) (m33 m) / det)
            (-(det3 (m01 m) (m02 m) (m03 m)
                    (m11 m) (m12 m) (m13 m)
                    (m21 m) (m22 m) (m23 m) / det))
              
            (-(det3 (m10 m) (m12 m) (m13 m)
                    (m20 m) (m22 m) (m23 m)
                    (m30 m) (m32 m) (m33 m) / det))
              (det3 (m00 m) (m02 m) (m03 m)
                    (m20 m) (m22 m) (m23 m)
                    (m30 m) (m32 m) (m33 m) / det)
            (-(det3 (m00 m) (m02 m) (m03 m)
                    (m10 m) (m12 m) (m13 m)
                    (m30 m) (m32 m) (m33 m) / det))
              (det3 (m00 m) (m02 m) (m03 m)
                    (m10 m) (m12 m) (m13 m)
                    (m20 m) (m22 m) (m23 m) / det)

              (det3 (m10 m) (m11 m) (m13 m)
                    (m20 m) (m21 m) (m23 m)
                    (m30 m) (m31 m) (m33 m) / det)
            (-(det3 (m00 m) (m01 m) (m03 m)
                    (m20 m) (m21 m) (m23 m)
                    (m30 m) (m31 m) (m33 m) / det))
              (det3 (m00 m) (m01 m) (m03 m)
                    (m10 m) (m11 m) (m13 m)
                    (m30 m) (m31 m) (m33 m) / det)
            (-(det3 (m00 m) (m01 m) (m03 m)
                    (m10 m) (m11 m) (m13 m)
                    (m20 m) (m21 m) (m23 m) / det))

            (-(det3 (m10 m) (m11 m) (m12 m)
                    (m20 m) (m21 m) (m22 m)
                    (m30 m) (m31 m) (m32 m) / det))
              (det3 (m00 m) (m01 m) (m02 m)
                    (m20 m) (m21 m) (m22 m)
                    (m30 m) (m31 m) (m32 m) / det)
            (-(det3 (m00 m) (m01 m) (m02 m)
                    (m10 m) (m11 m) (m12 m)
                    (m30 m) (m31 m) (m32 m) / det))
              (det3 (m00 m) (m01 m) (m02 m)
                    (m10 m) (m11 m) (m12 m)
                    (m20 m) (m21 m) (m22 m) / det)

transpose :: Matrix m => m -> m
transpose m = flip mIdxMap m $ \i j -> mElement j i m

maxVec :: (k ~ Scalar v, Ord k, Vector v) => v -> v -> v
maxVec = vZip max
minVec :: (k ~ Scalar v, Ord k, Vector v) => v -> v -> v
minVec = vZip min
minComponent :: (k ~ Scalar v, Ord k, Vector v) => v -> k
minComponent = vFold min
maxComponent :: (k ~ Scalar v, Ord k, Vector v) => v -> k
maxComponent = vFold max
minAbsComponent :: (k ~ Scalar v, Num k, Ord k, Vector v) => v -> k
minAbsComponent = vFold (\x y -> min (abs x) (abs y))
maxAbsComponent :: (k ~ Scalar v, Num k, Ord k, Vector v) => v -> k
maxAbsComponent = vFold (\x y -> max (abs x) (abs y))
vIndexOfMinComponent :: (k ~ Scalar v, Ord k, Vector v) => v -> Int
vIndexOfMinComponent = vIndexOf (<)
vIndexOfMaxComponent :: (k ~ Scalar v, Ord k, Vector v) => v -> Int
vIndexOfMaxComponent = vIndexOf (>)
vIndexOfMinAbsComponent :: (k ~ Scalar v, Ord k, Num k, Vector v)
                        => v -> Int
vIndexOfMinAbsComponent = vIndexOf (\x y -> abs x < abs y)
vIndexOfMaxAbsComponent :: (k ~ Scalar v, Ord k, Num k, Vector v)
                        => v -> Int
vIndexOfMaxAbsComponent = vIndexOf (\x y -> abs x > abs y)

-- |Cross product
{-# INLINE (<%>) #-}
(<%>) :: Floating a => Vec3 a -> Vec3 a -> Vec3 a
(Vec3 x1 y1 z1) <%> (Vec3 x2 y2 z2) = Vec3 (y1*z2 - z1*y2)
                                           (z1*x2 - x1*z2)
                                           (x1*y2 - y1*x2)

{-# INLINE unitVector #-}
unitVector :: (k ~ Scalar v, Floating k, Vector v) => v -> v
unitVector v = v </ len v

tripleProduct :: Floating a => Vec3 a -> Vec3 a -> Vec3 a -> a
tripleProduct v1 v2 v3 = (v1 <%> v2) <.> v3
