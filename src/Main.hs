module Main where

import Data.List
import Data.Maybe
import Data.Ray
import Data.RGB
import Data.Shape
import Data.Scene hiding (mkCamera)
import Data.Camera
import Data.HitRecord
import Data.VectorSpace
import Data.Luminaire
import Data.Bitmap
import Control.Monad ( forM_, when )
import System.Environment ( getArgs )
import System.IO ( hFlush, stdout )

main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) (error "Usage: raytracer <input.scene> <output.bmp>")
  let outfile = head (drop 1 args)
      input   = head args
      for = flip map
      comp x y = hrT x `compare` hrT y
      defaultCamera = mkCamera (Vec3 0.0 0.0 0.0)
                               (Vec3 0.0 0.0 (-1))
                               (Vec3 0 1 0)
                               2 (-2) 2 (-2) 2 1
      defaultDL = DirectedLight (Vec3 0 (-1) 0)
                                (Vec3 0.8 0.8 0.8)
      defaultAmbient = AmbientLight (Vec3 0.2 0.2 0.2)
  putStrLn $ "Reading scene: " ++ input
  shapes <- readSceneToShapes input
  c' <- readSceneToCamera input
  let (camera, nx, ny) = maybe (defaultCamera, 500, 500) id c'
  bmp <- allocBMP nx ny
  putStr $ "Rendering:"
  forM_ [(ny-1), (ny-2) .. 0] $ \j ->
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
          -- TODO: generalize this to work with multiple directed lights
          let cr       = hrColor hr
              cl       = dlColor defaultDL
              ca       = alColor defaultAmbient
              n        = hrNormal hr
              l        = unitVector $ negateV $ dlDirection defaultDL
              e        = unitVector $ negateV $ rayDirection r
              m        = max 0 (n <.> l)
              cp       = Vec3 1 1 1 <-> cr
              p        = 32
              h        = unitVector $ e <+> l
              hn       = h<.>n
              at       = rayOrigin r <+> (hrT hr*> rayDirection r)
              inShadow = or $ for shapes $ \shape ->
                shapeShadowHit shape (Ray at l) 0.0001 tmax 0
              c        = if inShadow
                           then cr <*> ca -- just the ambient light
                           else (cr <*> (ca <+> (m*>cl))) <+>
                                ((hn**p)*>(cl <*> cp))
          pokePixel i j ((toWord8 (clamp (getR c)))
                        ,(toWord8 (clamp (getG c)))
                        ,(toWord8 (clamp (getB c)))) bmp
        Nothing ->
          pokePixel i j ((toWord8 0.2), (toWord8 0.2), (toWord8 0.2)) bmp
  putStrLn "Done."
  putStrLn $ "Writing to: " ++ outfile
  writeBMP outfile bmp
  putStrLn "Done."
