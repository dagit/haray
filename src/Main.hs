module Main where

import Data.List
import Data.Maybe
import Data.Ray
import Data.RGB
import Data.Shape
import Data.HitRecord
import Data.VectorSpace
import Data.Bitmap
import Control.Monad ( forM_, when )
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  when (length args < 1) (error "Usage: raytracer <output.bmp>")
  let dir = Vec3 0 0 (-1)
      for = flip map
      shapes = [ mkSphere (SphereData (Vec3 250 250 (-1000))
                                      150
                                      (Vec3 0.2 0.2 0.8))
               , mkTriangle (TriangleData (Vec3 300 600 (-800))
                                          (Vec3 0 100 (-1000))
                                          (Vec3 450 20 (-1000))
                                          (Vec3 0.8 0.2 0.2)) ]::[Shape Float]
  bmp <- allocBMP 500 500 
  forM_ [499, 498 .. 0] $ \j ->
    forM_ [0 .. 499] $ \i -> do
      let tmax = 100000
          r = Ray (Vec3 (fromIntegral i) (fromIntegral j) 0) dir
          comp x y = hrT x `compare` hrT y
          hit = listToMaybe $ sortBy comp $ catMaybes $
            for shapes $ \shape -> shapeHit shape r 0.00001 tmax 0
      case hit of
        Just hr ->
          pokePixel i j ((toWord8 (clamp (getR (hrColor hr))))
                        ,(toWord8 (clamp (getG (hrColor hr))))
                        ,(toWord8 (clamp (getB (hrColor hr))))) bmp
        Nothing ->
          pokePixel i j ((toWord8 0.2), (toWord8 0.2), (toWord8 0.2)) bmp
  writeBMP (head args) bmp
