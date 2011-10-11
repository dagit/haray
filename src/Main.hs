module Main where

import Data.List
import Data.Maybe
import Data.Ray
import Data.RGB
import Data.Shape
import Data.Scene as S
import Data.Camera as C
import Data.HitRecord
import Data.VectorSpace
import Data.Luminaire
import Data.Bitmap
import Data.PNG
import Control.Monad ( forM_, when, mplus )
import System.Environment ( getArgs )
import System.IO ( hFlush, stdout )

main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) (error "Usage: raytracer <input.scene> <output.png>")
  let outfile  = head (drop 1 args)
      input    = head args
      for      = flip map
      comp x y = hrT x `compare` hrT y
      defaultCamera = (C.mkCamera (Vec3 0.0 0.0 0.0)
                                  (Vec3 0.0 0.0 (-1))
                                  (Vec3 0 1 0) 2 (-2) 2 (-2) 2 1, 500, 500)
      defaultDL = DirectedLight (Vec3 0 (-1) 0)
                                (Vec3 0.8 0.8 0.8)
      defaultAmbient = AmbientLight (Vec3 0.2 0.2 0.2)
  putStrLn $ "Reading scene: " ++ input
  scene <- readScene input
  let (camera, nx, ny) = maybe defaultCamera id c'
      c'               = S.mkCamera scene
      shapes           = mkShapes scene
      directedLights   = mkDirectedLights scene
      ambientLight     = maybe defaultAmbient id (mkAmbientLight scene)
  bmp <- allocBMP nx ny
  putStr $ "Rendering:"
  forM_ [0 .. (ny-1)] $ \j ->
    forM_ [0 .. (nx-1)] $ \i -> do
      let tmax = 100000
          r = getRay camera (((fromIntegral i)+0.5)/fromIntegral nx)
                            (((fromIntegral j)+0.5)/fromIntegral ny)
          hit = listToMaybe $ sortBy comp $ catMaybes $
            for shapes $ \shape -> shapeHit shape r 0.00001 tmax 0
          progress = i+j*nx
          tenpercent = fromIntegral nx * fromIntegral ny * 0.1
      when ((progress `mod` (round tenpercent)) == 0) (putStr ".")
      hFlush stdout
      case hit of
        Just hr -> do
          let
              ca       = alColor ambientLight
              cr       = hrHitTex hr (Vec2 0 0) {- TODO: implement UV -} at
              cp       = Vec3 1 1 1 <-> cr
              n        = hrNormal hr
              e        = unitVector $ negateV $ rayDirection r
              p        = 32
              at       = rayOrigin r <+> (hrT hr*> rayDirection r)
              vsum     = foldl' (<+>) (Vec3 0 0 0)
              c        = (cr <*> (ca <+> vsum diffs)) <+> vsum phongs
              (diffs, phongs) = unzip lightingTerms
              lightingTerms   = for directedLights $ \dl ->
                let  cl       = dlColor dl
                     l        = unitVector $ negateV $ dlDirection dl
                     h        = unitVector $ e <+> l
                     hn       = h<.>n
                     m        = max 0 (n <.> l)
                     inShadow = or $ for shapes $ \shape ->
                       shapeShadowHit shape (Ray at l) 0.01 tmax 0
                     c = if inShadow
                           then (Vec3 0 0 0, Vec3 0 0 0)
                           else (m*>cl, (hn**p)*>(cl<*>cp))
                in c
          pokePixel i j ((toWord8 (clamp (getR c)))
                        ,(toWord8 (clamp (getG c)))
                        ,(toWord8 (clamp (getB c)))) bmp
        Nothing ->
          pokePixel i j ((toWord8 0.2), (toWord8 0.2), (toWord8 0.2)) bmp
  putStrLn "Done."
  putStrLn $ "Writing to: " ++ outfile
  writePNG outfile bmp
  putStrLn "Done."
