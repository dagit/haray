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
      let nx = hmdHResolution hmd `div` 2
          ny = hmdVResolution hmd
          hmeters = abs (hmdHScreenSize hmd / 4 - hmdInterpupillaryDistance hmd / 2)
          h       = 4 * hmeters / hmdHScreenSize hmd
          xshift  = round (h * fromIntegral nx)
      img <- mkImage (nx*2) ny
      -- Left eye camera
      forM_ [0 .. (ny-1)] $ \j ->
        forM_ [0 .. (nx-1)] $ \i -> do
          let x = i + xshift
          hs <- forM [1..4::Int] $ \_ -> do -- 4 samples per pixel
            -- TODO: This really isn't a very good distribution        
            ry <- uniformR (-0.5,0.5) gen
            rx <- uniformR (-0.5,0.5) gen
            let tmax = 100000
                leftCam = camera { camCenter = camCenter camera <+> (Vec3 (hmdInterpupillaryDistance hmd / 2) 0 0)}
                r   = getRay leftCam (((fromIntegral i)+rx+0.5)/fromIntegral nx)
                                     (((fromIntegral j)+ry+0.5)/fromIntegral ny)
                hit = listToMaybe $ sortBy comp $ catMaybes $
                  for shapes $ \shape -> shapeHit shape r 0.00001 tmax 0
            return (r,hit)
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
          let x = i + nx - xshift
          hs <- forM [1..4::Int] $ \_ -> do -- 4 samples per pixel
            -- TODO: This really isn't a very good distribution        
            ry <- uniformR (-0.5,0.5) gen
            rx <- uniformR (-0.5,0.5) gen
            let tmax = 100000
                rightCam = camera { camCenter = camCenter camera <-> (Vec3 (hmdInterpupillaryDistance hmd / 2) 0 0)}
                r   = getRay rightCam (((fromIntegral i)+rx+0.5)/fromIntegral nx)
                                      (((fromIntegral j)+ry+0.5)/fromIntegral ny)
                hit = listToMaybe $ sortBy comp $ catMaybes $
                  for shapes $ \shape -> shapeHit shape r 0.00001 tmax 0
            return (r,hit)
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
                r = getRay camera (((fromIntegral i)+rx+0.5)/fromIntegral nx)
                                  (((fromIntegral j)+ry+0.5)/fromIntegral ny)
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
