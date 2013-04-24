module Graphics.Rendering.Haray.RGB where

import Numeric.LinearAlgebra.Vector
import Data.Word

type RGB a = Vec3 a

{-# SPECIALIZE INLINE getR :: RGB Double -> Double #-}
{-# SPECIALIZE INLINE getR :: RGB Float  -> Float  #-}
{-# SPECIALIZE INLINE getG :: RGB Double -> Double #-}
{-# SPECIALIZE INLINE getG :: RGB Float  -> Float  #-}
{-# SPECIALIZE INLINE getB :: RGB Double -> Double #-}
{-# SPECIALIZE INLINE getB :: RGB Float  -> Float  #-}
getR, getG, getB :: Floating a => RGB a -> a
getR = flip vElement 0
getG = flip vElement 1
getB = flip vElement 2

white, black :: Num a => RGB a
white = Vec3 1 1 1
black = Vec3 0 0 0

toWord8 :: (Enum a, Num a, Ord a) => a -> Word8
toWord8 a = toEnum . fromEnum $ 255 * clamp a
{-# SPECIALIZE INLINE toWord8 :: Double -> Word8 #-}
{-# SPECIALIZE INLINE toWord8 :: Float  -> Word8 #-}
{-# SPECIALIZE INLINE toWord8 :: Int    -> Word8 #-}

clamp :: (Num a, Ord a) => a -> a
clamp f | f >= 1 = 1
        | f <= 0 = 0
        | otherwise = f
{-# SPECIALIZE INLINE clamp :: Double -> Double #-}
{-# SPECIALIZE INLINE clamp :: Float  -> Float  #-}

gamma :: Fractional a => a
gamma = 2.2
{-# SPECIALIZE INLINE gamma :: Double #-}
{-# SPECIALIZE INLINE gamma :: Float  #-}

toWord8Gamma :: (Ord a, Enum a, Floating a) => a -> a -> Word8
toWord8Gamma g f = toEnum . fromEnum $ 255 * ((clamp f)**(1/g))
{-# SPECIALIZE INLINE toWord8Gamma :: Double -> Double -> Word8 #-}
{-# SPECIALIZE INLINE toWord8Gamma :: Float  -> Float  -> Word8 #-}
