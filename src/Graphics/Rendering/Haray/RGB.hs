module Graphics.Rendering.Haray.RGB where

import Numeric.LinearAlgebra.Vector
import Data.Word

type RGB a = Vec3 a

{-# INLINE getR #-}
{-# INLINE getG #-}
{-# INLINE getB #-}
getR, getG, getB :: Floating a => RGB a -> a
getR = flip vElement 0
getG = flip vElement 1
getB = flip vElement 2

white, black :: Num a => RGB a
white = Vec3 1 1 1
black = Vec3 0 0 0

{-# INLINE toWord8 #-}
toWord8 :: (Enum a, Num a, Ord a) => a -> Word8
toWord8 a = toEnum . fromEnum $ 255 * clamp a

{-# INLINE clamp #-}
clamp :: (Num a, Ord a) => a -> a
clamp f | f >= 1 = 1
        | f <= 0 = 0
        | otherwise = f

gamma :: Fractional a => a
gamma = 2.2

toWord8Gamma :: (Ord a, Enum a, Floating a) => a -> a -> Word8
toWord8Gamma g f = toEnum . fromEnum $ 255 * ((clamp f)**(1/g))