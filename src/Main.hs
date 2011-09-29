module Main where

import Data.List
import Data.Maybe
import Data.Ray
import Data.RGB
import Data.Shape
import Data.HitRecord
import Data.VectorSpace
import Data.Bitmap
import Control.Monad ( forM_ )

main :: IO ()
main = do
  let dir = Vec3 0 0 (-1)
      for = flip map
      shapes = [ mkSphere (SphereData (Vec3 250 250 (-1000))
                                      150
                                      (Vec3 0.2 0.2 0.8))
               , mkTriangle (TriangleData (Vec3 300 600 (-800))
                                          (Vec3 0 100 (-1000))
                                          (Vec3 450 20 (-1000))
                                          (Vec3 0.8 0.2 0.2)) ]
  bmp <- allocBMP 500 500 
  forM_ [499, 498 .. 0] $ \j ->
    forM_ [0 .. 499] $ \i -> do
      let tmax = 100000
          r = Ray (Vec3 (fromIntegral i) (fromIntegral j) 0) dir
          keepClosest :: Ord a => [Maybe (HitRecord a)]
                      -> Maybe (HitRecord a)
          keepClosest [Nothing, Nothing] = Nothing
          keepClosest [Nothing, h] = h
          keepClosest [h, Nothing] = h
          keepClosest [Just h1, Just h2] | (hrT h1) <= (hrT h2) = Just h1
                                         | otherwise = Just h2
          hit = keepClosest $ for shapes $ \shape ->
            case shapeHit shape r 0.00001 tmax 0 of
              Just rec -> Just rec
              Nothing -> Nothing
      case hit of
        Just hr ->
          pokePixel i j ((toWord8 (clamp (getR (hrColor hr))))
                        ,(toWord8 (clamp (getG (hrColor hr))))
                        ,(toWord8 (clamp (getB (hrColor hr))))) bmp
        Nothing ->
          pokePixel i j ((toWord8 0.2), (toWord8 0.2), (toWord8 0.2)) bmp
  writeBMP "raytracer.bmp" bmp
