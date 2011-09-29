module Main where

import Data.List
import Data.Maybe
import Data.Ray
import Data.RGB
import Data.Shape
import Data.Scene
import Data.HitRecord
import Data.VectorSpace
import Data.Bitmap
import Control.Monad ( forM_, when )
import System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) (error "Usage: raytracer <input.scene> <output.bmp>")
  let outfile = head (drop 1 args)
      input   = head args
      dir = Vec3 0 0 (-1)
      for = flip map
  putStrLn $ "Reading scene: " ++ input
  shapes <- readSceneToShapes input
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
  writeBMP outfile bmp
