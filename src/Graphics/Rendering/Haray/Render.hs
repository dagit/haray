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
import Codec.Picture.Types ( Image(..), freezeImage, PixelRGB8(..) )
import Codec.Picture ( writePng )

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
  let (camera, nx, ny) = maybe defaultCamera id c'
      c'               = S.mkCamera scene
      hmdi             = mkHMDInfo scene
      directedLights   = mkDirectedLights scene
      ambientLight     = maybe defaultAmbient id (mkAmbientLight scene)
      comp x y         = hrT x `compare` hrT y
      -- TODO: make these defaults configurable
      defaultCamera    = (C.mkCamera (Vec3 0.0 0.0 0.0)
                                     (Vec3 0.0 0.0 (-1))
                                     (Vec3 0 1 0) 2 (-2) 2 (-2) 2 1, 500, 500)
      -- defaultDL = DirectedLight (Vec3 0 (-1) 0)
      --                          (Vec3 0.8 0.8 0.8 :: Vec3 Double)
      defaultAmbient = AmbientLight (Vec3 0.2 0.2 0.2)
  case hmdi of
    Just hmd -> do
      -- moved here to use unshadowed nx and ny
      let nx = hmdHResolution hmd `div` 2
          ny = hmdVResolution hmd
          xsz = hmdHScreenSize hmd / 2
          halfIPD = hmdInterpupillaryDistance hmd / 2
          ppm = fromIntegral nx / xsz
          viewCenter = hmdHScreenSize hmd / 4
          eyeProjectionShift = viewCenter - hmdInterpupillaryDistance hmd / 2
          -- TODO: This might be wrong still, but based on what I'm seeing in the rift
          -- documentation I can't tell for sure.
          !xshift = round (eyeProjectionShift * ppm)
          -- xshift = 0
      img <- mkImage (nx*2) ny
      -- Left eye camera
      forM_ [0 .. (ny-1)] $ \j ->
        forM_ [0 .. (nx-1)] $ \i -> do
          let x  = i + xshift
          hs <- forM [1..4::Int] $ \_ -> do -- 4 samples per pixel
            -- TODO: This really isn't a very good distribution        
            ry <- uniformR (-0.5,0.5) gen
            rx <- uniformR (-0.5,0.5) gen
            let tmax = 100000
                Vec4 k0 k1 k2 k3 = hmdDistortionK hmd
                xn = 2*(((fromIntegral i)+rx+0.5)/fromIntegral nx) - 1
                yn = 2*(((fromIntegral j)+rx+0.5)/fromIntegral ny) - 1
                rSq  = xn*xn + yn*yn
                xrad = xn*(k0 + k1*rSq + k2*rSq*rSq + k3*rSq*rSq*rSq)
                yrad = yn*(k0 + k1*rSq + k2*rSq*rSq + k3*rSq*rSq*rSq)
                radius = sqrt (xn*xn + yn*yn)
                leftCam = translateCamera camera (Vec3 halfIPD 0 0)
                black = \_ _ -> Vec3 0 0 0
                r   = getBarrelRay leftCam (hmdDistortionK hmd)
                                            ((fromIntegral i)+rx+0.5)
                                            ((fromIntegral j)+ry+0.5)
                                            (fromIntegral nx)
                                            (fromIntegral ny)
                hit = listToMaybe $ sortBy comp $ catMaybes $
                  for shapes $ \shape -> shapeHit shape r 0.00001 tmax 0
            if (-1.25 <= xrad && xrad <= 1.25 && -1.25 <= yrad && yrad <= 1.25)
              then return (r,hit)
              else return (r,Just (HitRecord { hrT = 0.0, hrNormal = Vec3 0 0 0, hrUV = Vec2 0 0, hrHitP = Vec3 0 0 0, hrHitTex = black } ))
{- Need this when using the fish eye view
            if (radius <= 1)
              then return (r,hit)
              else return (r,Nothing)
-}
          let cs   = map (processHit shapes directedLights ambientLight) hs
              avgC = foldl1' (<+>) cs </ genericLength cs
          -- it's the left eye so write to the pixel coordinates of the final image
          -- without shifting
          when (0 <= x && x < nx) $ writePixelRGB img x j (PixelRGB8 (toWord8 (clamp (getR avgC)))
                                                                     (toWord8 (clamp (getG avgC)))
                                                                     (toWord8 (clamp (getB avgC))))
      -- Right eye camera
      forM_ [0 .. (ny-1)] $ \j ->
        forM_ [0 .. (nx-1)] $ \i -> do
          let x  = i + nx - xshift
          hs <- forM [1..4::Int] $ \_ -> do -- 4 samples per pixel
            -- TODO: This really isn't a very good distribution        
            ry <- uniformR (-0.5,0.5) gen
            rx <- uniformR (-0.5,0.5) gen
            let tmax = 100000
                Vec4 k0 k1 k2 k3 = hmdDistortionK hmd
                rightCam = translateCamera camera (Vec3 (negate halfIPD) 0 0)
                xn = 2*(((fromIntegral i)+rx+0.5)/fromIntegral nx) - 1
                yn = 2*(((fromIntegral j)+rx+0.5)/fromIntegral ny) - 1
                rSq  = xn*xn + yn*yn
                xrad = xn*(k0 + k1*rSq + k2*rSq*rSq + k3*rSq*rSq*rSq)
                yrad = yn*(k0 + k1*rSq + k2*rSq*rSq + k3*rSq*rSq*rSq)
                radius = sqrt (xn*xn + yn*yn)
                black = \_ _ -> Vec3 0 0 0
                r   = getBarrelRay rightCam  (hmdDistortionK hmd)
                                             ((fromIntegral i)+rx+0.5)
                                             ((fromIntegral j)+ry+0.5)
                                             (fromIntegral nx)
                                             (fromIntegral ny)
                hit = listToMaybe $ sortBy comp $ catMaybes $
                  for shapes $ \shape -> shapeHit shape r 0.00001 tmax 0
            if (-1.25 <= xrad && xrad <= 1.25 && -1.25 <= yrad && yrad <= 1.25)
              then return (r,hit)
              else return (r,Just (HitRecord { hrT = 0.0, hrNormal = Vec3 0 0 0, hrUV = Vec2 0 0, hrHitP = Vec3 0 0 0, hrHitTex = black } ))
{- Need this when using the fish eye view
            if (radius <= 1)
              then return (r,hit)
              else return (r,Nothing)
-}
          let cs   = map (processHit shapes directedLights ambientLight) hs
              avgC = foldl1' (<+>) cs </ genericLength cs
          -- shift the pixel location of the final image by nx
          when (nx <= x && x < 2*nx) $ writePixelRGB img x j (PixelRGB8 (toWord8 (clamp (getR avgC)))
                                                                        (toWord8 (clamp (getG avgC)))
                                                                        (toWord8 (clamp (getB avgC))))
      img' <- freezeImage img
      return img'
    Nothing  -> do
      img <- mkImage nx ny
      forM_ [0 .. (ny-1)] $ \j ->
        forM_ [0 .. (nx-1)] $ \i -> do
          hs <- forM [1..4::Int] $ \_ -> do -- 4 samples per pixel
            -- TODO: This really isn't a very good distribution        
            ry <- uniformR (-0.5,0.5) gen
            rx <- uniformR (-0.5,0.5) gen
            let tmax = 100000
                r = getPerspectiveRay camera ((fromIntegral i)+rx+0.5)
                                             ((fromIntegral j)+ry+0.5)
                                             (fromIntegral nx)
                                             (fromIntegral ny)
                hit = listToMaybe $ sortBy comp $ catMaybes $
                  for shapes $ \shape -> shapeHit shape r 0.00001 tmax 0
            return (r,hit)
          let cs   = map (processHit shapes directedLights ambientLight) hs
              avgC = foldl1' (<+>) cs </ genericLength cs
          writePixelRGB img i j (PixelRGB8 (toWord8 (clamp (getR avgC)))
                                           (toWord8 (clamp (getG avgC)))
                                           (toWord8 (clamp (getB avgC))))
      img' <- freezeImage img
      return img'

processHit :: [Shape RealTy] -> [DirectedLight RealTy] -> AmbientLight RealTy
           -> (Ray RealTy, Maybe (HitRecord RealTy)) -> RGB RealTy
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
