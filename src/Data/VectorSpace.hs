{-# LANGUAGE TypeFamilies #-}
module Data.VectorSpace where

data Vec3 a = Vec3 !a !a !a
  deriving (Read, Show, Eq, Ord)
data Vec2 a = Vec2 !a !a
  deriving (Read, Show, Eq, Ord)

class VectorSpace v where
  type Scalar v :: *
  {-# INLINE element #-}
  element :: (k ~ Scalar v) => Int -> v -> k
  {-# INLINE indexOf #-}
  indexOf :: (k ~ Scalar v, Ord k) => (k -> k -> Bool) -> v -> Int
  {-# INLINE zipV #-}
  zipV  :: (k ~ Scalar v) => (k -> k -> k) -> v -> v -> v
  {-# INLINE mapV #-}
  mapV  :: (k ~ Scalar v) => (k -> k) -> v -> v
  {-# INLINE foldV #-}
  foldV :: (k ~ Scalar v) => (k -> k -> k) -> v -> k

instance Floating a => VectorSpace (Vec3 a) where
  type Scalar (Vec3 a) = a
  element 0 (Vec3 x _ _) = x
  element 1 (Vec3 _ y _) = y
  element 2 (Vec3 _ _ z) = z
  element i _ = error ("Index " ++ show i ++ ": out of range, must be 0 to 2")
  mapV f (Vec3 x y z) = Vec3 (f x) (f y) (f z)
  zipV f (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (f x1 x2) (f y1 y2) (f z1 z2)
  foldV f (Vec3 x y z) = f (f x y) z
  indexOf p (Vec3 x y z) | z `p` x && z `p` y = 2
                         | y `p` x && y `p` z = 1
                         | otherwise = 0

instance Floating a => VectorSpace (Vec2 a) where
  type Scalar (Vec2 a) = a
  element 0 (Vec2 x _) = x
  element 1 (Vec2 _ y) = y
  element i _ = error ("Index " ++ show i ++ ": out of range, must be 0 or 1")
  mapV f (Vec2 x y) = Vec2 (f x) (f y)
  zipV f (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (f x1 x2) (f y1 y2)
  foldV f (Vec2 x y) = f x y
  indexOf p (Vec2 x y) | y `p` x   = 1
                       | otherwise = 0

{-# INLINE negateV #-}
negateV :: (k ~ Scalar v, Num k, VectorSpace v) => v -> v
negateV = mapV negate
{-# INLINE (<+>) #-}
(<+>) :: (k ~ Scalar v, Num k, VectorSpace v) => v -> v -> v
(<+>) = zipV (+)
{-# INLINE (<*>) #-}
(<*>) :: (k ~ Scalar v, Num k, VectorSpace v) => v -> v -> v
(<*>) = zipV (*)
{-# INLINE (<->) #-}
(<->) :: (k ~ Scalar v, Num k, VectorSpace v) => v -> v -> v
(<->) = zipV (-)
{-# INLINE (</>) #-}
(</>) :: (k ~ Scalar v, Fractional k, VectorSpace v) => v -> v -> v
(</>) = zipV (/)
{-# INLINE (<.>) #-}
(<.>) :: (k ~ Scalar v, Num k, VectorSpace v) => v -> v -> k -- dot product
v1 <.> v2 = foldV (+) (v1 <*> v2)
{-# INLINE (*>) #-}
(*>) :: (k ~ Scalar v, Num k, VectorSpace v) => k -> v -> v
k *> v = mapV (k*) v
{-# INLINE (</) #-}
(</) :: (k ~ Scalar v, Fractional k, VectorSpace v) => v -> k -> v
v </ k = mapV (/k) v
{-# INLINE len #-}
len :: (k ~ Scalar v, Floating k, VectorSpace v) => v -> k
len v = sqrt $ lenSquared v
{-# INLINE lenSquared #-}
lenSquared :: (k ~ Scalar v, Num k, VectorSpace v) => v -> k
lenSquared v = v <.> v

maxVec :: (k ~ Scalar v, Ord k, VectorSpace v) => v -> v -> v
maxVec = zipV max
minVec :: (k ~ Scalar v, Ord k, VectorSpace v) => v -> v -> v
minVec = zipV min
minComponent :: (k ~ Scalar v, Ord k, VectorSpace v) => v -> k
minComponent = foldV min
maxComponent :: (k ~ Scalar v, Ord k, VectorSpace v) => v -> k
maxComponent = foldV max
minAbsComponent :: (k ~ Scalar v, Num k, Ord k, VectorSpace v) => v -> k
minAbsComponent = foldV (\x y -> min (abs x) (abs y))
maxAbsComponent :: (k ~ Scalar v, Num k, Ord k, VectorSpace v) => v -> k
maxAbsComponent = foldV (\x y -> max (abs x) (abs y))
indexOfMinComponent :: (k ~ Scalar v, Ord k, VectorSpace v) => v -> Int
indexOfMinComponent = indexOf (<)
indexOfMaxComponent :: (k ~ Scalar v, Ord k, VectorSpace v) => v -> Int
indexOfMaxComponent = indexOf (>)
indexOfMinAbsComponent :: (k ~ Scalar v, Ord k, Num k, VectorSpace v)
                       => v -> Int
indexOfMinAbsComponent = indexOf (\x y -> abs x < abs y)
indexOfMaxAbsComponent :: (k ~ Scalar v, Ord k, Num k, VectorSpace v)
                       => v -> Int
indexOfMaxAbsComponent = indexOf (\x y -> abs x > abs y)

-- |Cross product
{-# INLINE (<%>) #-}
(<%>) :: Floating a => Vec3 a -> Vec3 a -> Vec3 a
(Vec3 x1 y1 z1) <%> (Vec3 x2 y2 z2) = Vec3 (y1*z2 - z1*y2)
                                           (z1*x2 - x1*z2)
                                           (x1*y2 - y1*x2)

{-# INLINE unitVector #-}
unitVector :: (k ~ Scalar v, Floating k, VectorSpace v) => v -> v
unitVector v = v </ len v

tripleProduct :: Floating a => Vec3 a -> Vec3 a -> Vec3 a -> a
tripleProduct v1 v2 v3 = (v1 <%> v2) <.> v3
