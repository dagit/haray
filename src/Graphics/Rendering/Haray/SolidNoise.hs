module Graphics.Rendering.Haray.SolidNoise where

{-# SPECIALIZE omega :: Double -> Double #-}
{-# SPECIALIZE omega :: Float -> Float #-}
omega :: (Ord a, Floating a) => a -> a
omega t | t < 0 = omega (negate t)
omega t = (-6)*t*t*t*t*t + 15*t*t*t*t + (-10)*t*t*t + 1
