module Graphics.Rendering.Haray.Scene where

import Data.Maybe
-- TODO: sort out the smart constructor name mess...use a real parser?
import Graphics.Rendering.Haray.Shape hiding ( mkTriangle, mkSphere, mkPlane )
import qualified Graphics.Rendering.Haray.Shape as Shape (mkTriangle, mkSphere, mkPlane)
import qualified System.IO.Strict as S
import qualified Graphics.Rendering.Haray.Camera as C
import Numeric.LinearAlgebra.Vector
import Graphics.Rendering.Haray.RGB
import Graphics.Rendering.Haray.Texture
import Graphics.Rendering.Haray.Luminaire
import Control.Applicative

type Scene = [SceneElement]

data SceneElement = SESphere (Sphere Float)
                  | SETriangle (Triangle Float)
                  | SEPlane (Plane Float)
                  | SECamera { camEye       :: Vec3 Float
                             , camGaze      :: Vec3 Float
                             , camUp        :: Vec3 Float
                             , camU0        :: Float
                             , camV0        :: Float
                             , camU1        :: Float
                             , camV1        :: Float
                             , camDist      :: Float
                             , camNX        :: Int
                             , camNY        :: Int
                             , camApeture   :: Float }
                  | SEDirectedLight (DirectedLight Float)
                  | SEAmbientLight (AmbientLight Float)
  deriving (Read, Show, Eq, Ord)

data TextureDescription a = Matte (RGB a)
  | Stripe
  | BWNoise
  deriving (Read, Show, Eq, Ord)

mkTexture :: (Ord a, RealFrac a, Floating a) => TextureDescription a -> IO (Texture a)
mkTexture (Matte rgb) = return (mkMatteTexture (MatteData rgb))
mkTexture Stripe      = return mkStripeTexture
mkTexture BWNoise     = mkBWNoiseTexture

data Triangle a = Triangle
  { tP0      :: Vec3 a
  , tP1      :: Vec3 a
  , tP2      :: Vec3 a
  , tTexture :: TextureDescription a
  } deriving (Read, Show, Eq, Ord)

mkTriangle :: (Ord a, RealFrac a, Floating a) => Triangle a -> IO (TriangleData a)
mkTriangle (Triangle p0 p1 p2 tex) = do
  tex' <- mkTexture tex
  return (TriangleData
    { tdP0  = p0
    , tdP1  = p1
    , tdP2  = p2
    , tdTex = tex' })

data Sphere a = Sphere
  { sCenter  :: Vec3 a
  , sRadius  :: a
  , sTexture :: TextureDescription a
  } deriving (Read, Show, Eq, Ord)

mkSphere :: (Ord a, RealFrac a, Floating a) => Sphere a -> IO (SphereData a)
mkSphere (Sphere c r tex) = do
  tex' <- mkTexture tex
  return (SphereData
    { sphereCenter = c
    , sphereRadius = r
    , sphereTex    = tex' })

data Plane a = Plane
  { pCenter  :: Vec3 a
  , pNormal  :: Vec3 a
  , pTexture :: TextureDescription a
  } deriving (Read, Show, Eq, Ord)

mkPlane :: (Ord a, RealFrac a, Floating a) => Plane a -> IO (PlaneData a)
mkPlane (Plane c n tex) = do
  tex' <- mkTexture tex
  return (PlaneData
    { pdCenter  = c
    , pdNormal  = n
    , pdTex     = tex' })

mkShape :: SceneElement -> IO (Maybe (Shape Float))
mkShape (SESphere sd)   = do
  s <- mkSphere sd
  return $ Just $ Shape.mkSphere s
mkShape (SETriangle td) = do
  t <- mkTriangle td
  return $ Just $ Shape.mkTriangle t
mkShape (SEPlane pd)    = do
  p <- mkPlane pd
  return $ Just $ Shape.mkPlane p
mkShape _               = return Nothing

mkShapes :: Scene -> IO [Shape Float]
mkShapes scene = catMaybes <$> mapM mkShape scene

readScene :: FilePath -> IO Scene
readScene fp = do
  cs <- S.readFile fp
  return (read cs)

mkCamera :: Scene -> Maybe (C.Camera Float, Int, Int)
mkCamera = listToMaybe . catMaybes . map mkCamera'
  where
  mkCamera' :: SceneElement -> Maybe (C.Camera Float, Int, Int)
  mkCamera' c@(SECamera{}) = Just (C.mkCamera (camEye c)
                                              (camGaze c)
                                              (camUp c)
                                              (camApeture c)
                                              (camU0 c)
                                              (camU1 c)
                                              (camV0 c)
                                              (camV1 c)
                                              (camDist c), camNX c, camNY c)
  mkCamera' _            = Nothing

readSceneToShapes :: FilePath -> IO [Shape Float]
readSceneToShapes fp = do
  sc <- readScene fp
  mkShapes sc

readSceneToCamera :: FilePath -> IO (Maybe (C.Camera Float, Int, Int))
readSceneToCamera fp = do
  sc <- readScene fp
  return (mkCamera sc)

mkDirectedLights :: Scene -> [DirectedLight Float]
mkDirectedLights = catMaybes . map mkDirectedLights'
  where
  mkDirectedLights' :: SceneElement -> Maybe (DirectedLight Float)
  mkDirectedLights' (SEDirectedLight l) = Just l 
  mkDirectedLights' _                   = Nothing

mkAmbientLight :: Scene -> Maybe (AmbientLight Float)
mkAmbientLight = listToMaybe . catMaybes . map mkAmbientLight'
  where
  mkAmbientLight' :: SceneElement -> Maybe (AmbientLight Float)
  mkAmbientLight' (SEAmbientLight l) = Just l 
  mkAmbientLight' _                  = Nothing

