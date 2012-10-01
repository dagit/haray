module Main where

import Data.List
import Data.Maybe
import Graphics.Rendering.Haray.Ray
import Graphics.Rendering.Haray.RGB
import Graphics.Rendering.Haray.Shape
import Graphics.Rendering.Haray.Scene as S
import Graphics.Rendering.Haray.Camera as C
import Graphics.Rendering.Haray.HitRecord
import Graphics.Rendering.Haray.Luminaire
import Graphics.Rendering.Haray.Bitmap
import Numeric.LinearAlgebra.Vector
import Control.Monad ( forM_, when )
import Control.Monad.ST ( stToIO )
import System.Environment ( getArgs )
import System.IO ( hFlush, stdout )
import Codec.Picture.Types

main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) (error "Usage: haray <input.scene> <output.png>")
  let outfile  = head (drop 1 args)
      input    = head args
      for      = flip map
      comp x y = hrT x `compare` hrT y
      defaultCamera = (C.mkCamera (Vec3 0.0 0.0 0.0)
                                  (Vec3 0.0 0.0 (-1))
                                  (Vec3 0 1 0) 2 (-2) 2 (-2) 2 1, 500, 500)
      -- defaultDL = DirectedLight (Vec3 0 (-1) 0)
      --                          (Vec3 0.8 0.8 0.8 :: Vec3 Double)
      defaultAmbient = AmbientLight (Vec3 0.2 0.2 0.2)
  putStrLn $ "Reading scene: " ++ input
  scene <- readScene input
  let (camera, nx, ny) = maybe defaultCamera id c'
      c'               = S.mkCamera scene
      shapes           = mkShapes scene
      directedLights   = mkDirectedLights scene
      ambientLight     = maybe defaultAmbient id (mkAmbientLight scene)
  bmp <- stToIO $ mkImage nx ny
  putStr $ "Rendering:"
  forM_ [0 .. (ny-1)] $ \j ->
    forM_ [0 .. (nx-1)] $ \i -> do
      let tmax = 100000
          r = getRay camera (((fromIntegral i)+0.5)/fromIntegral nx)
                            (((fromIntegral j)+0.5)/fromIntegral ny)
          hit = listToMaybe $ sortBy comp $ catMaybes $
            for shapes $ \shape -> shapeHit shape r 0.00001 tmax 0
          progress = i+j*nx
          tenpercent = fromIntegral nx * fromIntegral ny * (0.1::Double)
      when ((progress `mod` (round tenpercent)) == 0) (putStr ".")
      hFlush stdout
      case hit of
        Just hr -> do
          let
              ca       = alColor ambientLight
              cr       = hrHitTex hr (Vec2 0 0) {- TODO: implement UV -} at
              cp       = Vec3 1 1 1 <-> cr
              n        = hrNormal hr
              e        = unitVector $ vNegate $ rayDirection r
              p        = 32
              at       = rayOrigin r <+> (hrT hr*> rayDirection r)
              vsum     = foldl' (<+>) (Vec3 0 0 0)
              c        = (cr <*> (ca <+> vsum diffs)) <+> vsum phongs
              (diffs, phongs) = unzip lightingTerms
              lightingTerms   = for directedLights $ \dl ->
                let  cl       = dlColor dl
                     l        = unitVector $ vNegate $ dlDirection dl
                     h        = unitVector $ e <+> l
                     hn       = h<.>n
                     m        = max 0 (n <.> l)
                     inShadow = or $ for shapes $ \shape ->
                       shapeShadowHit shape (Ray at l) 0.01 tmax 0
                     sc = if inShadow
                           then (Vec3 0 0 0, Vec3 0 0 0)
                           else (m*>cl, (hn**p)*>(cl<*>cp))
                in sc
          writePixelRGBIO bmp i j (PixelRGB8 (toWord8 (clamp (getR c)))
                                             (toWord8 (clamp (getG c)))
                                             (toWord8 (clamp (getB c))))
        Nothing ->
          writePixelRGBIO bmp i j (PixelRGB8 (toWord8 (0.2::Double)) (toWord8 (0.2::Double)) (toWord8 (0.2::Double)))
  putStrLn "Done."
  putStrLn $ "Writing to: " ++ outfile
  writePngRGB8 outfile bmp
  putStrLn "Done."
