{-# LANGUAGE BangPatterns #-}

module Graphics.Rendering.Haray.Render where


import Data.List
import Data.Maybe
import Graphics.Rendering.Haray.Ray
import Graphics.Rendering.Haray.RGB
import Graphics.Rendering.Haray.Shape
import Graphics.Rendering.Haray.Scene as S
import Graphics.Rendering.Haray.Camera as C
import Graphics.Rendering.Haray.HitRecord
import Graphics.Rendering.Haray.HMDInfo
import Graphics.Rendering.Haray.Luminaire
import Graphics.Rendering.Haray.Bitmap
import Numeric.LinearAlgebra.Vector
import Control.Monad ( forM_, forM, when )
import Control.Monad.ST
import System.Random.MWC
import Codec.Picture.Types ( Image(..), unsafeFreezeImage, PixelRGB8(..) )
import Codec.Picture ( writePng, writePixel, pixelAt )

renderSceneFromTo :: FilePath -> FilePath -> IO ()
renderSceneFromTo from to = do
  img <- renderSceneFromFile from 
  putStrLn $ "Writing to: " ++ to
  writePng to img

renderSceneFromFile :: FilePath -> IO (Image PixelRGB8)
renderSceneFromFile from = do
  putStrLn $ "Reading scene: " ++ from
  scene <- readScene from
  putStr $ "Rendering:"
  withSystemRandom (\gen -> renderScene gen scene)

for :: [a] -> (a -> b) -> [b]
for = flip map

renderScene :: GenST s -> [SceneElement] -> ST s (Image PixelRGB8)
renderScene gen scene = do
  shapes <- mkShapes gen scene
  let camera         = maybe defaultCamera id c'
      nx             = C.camNX camera
      ny             = C.camNY camera
      c'             = S.mkCamera scene
      hmdi           = mkHMDInfo scene
      directedLights = mkDirectedLights scene
      ambientLight   = maybe defaultAmbient id (mkAmbientLight scene)
      defaultCamera  = (C.mkCamera C.Pinhole
                                   (Vec3 0.0 0.0 0.0)
                                   (Vec3 0.0 0.0 (-1))
                                   (Vec3 0 1 0) 1 45 500 500)
      defaultAmbient = AmbientLight (Vec3 0.2 0.2 0.2)
  case hmdi of
    Just hmd -> do
      -- moved here to use unshadowed nx and ny
      let nx' = hmdHResolution hmd `div` 2
          ny' = hmdVResolution hmd
          xsz = hmdHScreenSize hmd / 2
          halfIPD = hmdInterpupillaryDistance hmd / 2
          leftCam = translateCamera camera (Vec3 halfIPD 0 0)
          rightCam = translateCamera camera (Vec3 halfIPD 0 0)
          ppm = fromIntegral nx' / xsz
          viewCenter = hmdHScreenSize hmd / 4
          eyeProjectionShift = viewCenter - hmdInterpupillaryDistance hmd / 2
          -- TODO: This might be wrong still, but based on what I'm seeing in the rift
          -- documentation I can't tell for sure.
          !xshift = round (eyeProjectionShift * ppm)
      leftImg  <- renderWith leftCam  shapes directedLights ambientLight gen nx' ny'
      rightImg <- renderWith rightCam shapes directedLights ambientLight gen nx' ny'
      -- Join the left and right images
      img <- mkImage (nx'*2) ny'
      forM_ [0..ny'-1] $ \y ->
        forM_ [0..nx'-1] $ \x -> do
          let leftx'  = x + xshift
              rightx' = x + nx' - xshift
          -- shift things over to match eye position on physical screen
          when (0   <= leftx'  && leftx'  < nx')   $ writePixel img leftx'  y (pixelAt leftImg  x y)
          when (nx' <= rightx' && rightx' < 2*nx') $ writePixel img rightx' y (pixelAt rightImg x y)
      img' <- unsafeFreezeImage img
      return img'
    Nothing -> renderWith camera shapes directedLights ambientLight gen nx ny

renderWith :: (Variate a, Enum a, Eq a, Ord a, Floating a)
           => Camera a -> [Shape a] -> [DirectedLight a] -> AmbientLight a
           -> GenST s -> Int -> Int -> ST s (Image PixelRGB8)
renderWith cam shapes directedLights ambientLight gen nx ny = do
  let tmax = 100000
      comp x y = hrT x `compare` hrT y
  withImage nx ny $ \i j -> do
    rs <- forM [1..4::Int] $ \_ -> do -- 4 samples per pixel
      -- TODO: This really isn't a very good distribution
      ry <- uniformR (-0.9,0.9) gen
      rx <- uniformR (-0.9,0.9) gen
      return (camRay cam ((fromIntegral i)+rx) ((fromIntegral j)+ry))
    let hs = for (catMaybes rs) $ \r -> (r, listToMaybe $ sortBy comp $ catMaybes $
               for shapes $ \shape ->
                 shapeHit shape r 0.00001 tmax 0)
    let cs    = map (processHit shapes directedLights ambientLight) hs
        avgC  = case genericLength cs of
                  0     -> black
                  lenCs -> foldl' (<+>) black cs </ lenCs
    return (PixelRGB8 (toWord8 (clamp (getR avgC)))
                      (toWord8 (clamp (getG avgC)))
                      (toWord8 (clamp (getB avgC))))
{-# SPECIALIZE renderWith :: Camera Double -> [Shape Double] -> [DirectedLight Double]
                          -> AmbientLight Double -> GenST s -> Int -> Int -> ST s (Image PixelRGB8) #-}
{-# SPECIALIZE renderWith :: Camera Float -> [Shape Float] -> [DirectedLight Float]
                          -> AmbientLight Float -> GenST s -> Int -> Int -> ST s (Image PixelRGB8) #-}

processHit :: (Floating a, Fractional a, Ord a)
           => [Shape a] -> [DirectedLight a] -> AmbientLight a
           -> (Ray a, Maybe (HitRecord a)) -> RGB a
processHit shapes directedLights ambientLight (r,hit) = case hit of
  Just hr ->
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
        tmax     = 100000 -- TODO: hack refactor me
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
    in c
  Nothing -> Vec3 0.2 0.2 0.2
{-# SPECIALIZE processHit :: [Shape Double] -> [DirectedLight Double] -> AmbientLight Double
                          -> (Ray Double, Maybe (HitRecord Double)) -> RGB Double #-}
{-# SPECIALIZE processHit :: [Shape Float] -> [DirectedLight Float] -> AmbientLight Float
                          -> (Ray Float, Maybe (HitRecord Float)) -> RGB Float #-}
