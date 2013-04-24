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
import Data.Vector.Unboxed (Unbox)

type Scene = [SceneElement]

type RealTy = Float


data SceneElement = SESphere (Sphere RealTy)
                  | SETriangle (Triangle RealTy)
                  | SEPlane (Plane RealTy)
                  | SECamera { camEye       :: Vec3 RealTy
                             , camGaze      :: Vec3 RealTy
                             , camUp        :: Vec3 RealTy
                             , camU0        :: RealTy
                             , camV0        :: RealTy
                             , camU1        :: RealTy
                             , camV1        :: RealTy
                             , camDist      :: RealTy
                             , camNX        :: Int
                             , camNY        :: Int
                             , camApeture   :: RealTy }
                  | SEDirectedLight (DirectedLight RealTy)
                  | SEAmbientLight (AmbientLight RealTy)
  deriving (Read, Show, Eq, Ord)

data TextureDescription a = Matte (RGB a)
  | Stripe
  | BWNoise
  | Marble !a
  deriving (Read, Show, Eq, Ord)

mkTexture :: (RealFloat a, Unbox a, Ord a, RealFrac a, Floating a) => TextureDescription a -> IO (Texture a)
mkTexture (Matte rgb) = return (mkMatteTexture (MatteData rgb))
mkTexture Stripe      = return mkStripeTexture
mkTexture BWNoise     = mkBWNoiseTexture
mkTexture (Marble s)  = mkMarbleData s >>= (return . mkMarbleTexture)
{-# SPECIALIZE mkTexture :: TextureDescription Double -> IO (Texture Double) #-}
{-# SPECIALIZE mkTexture :: TextureDescription Float  -> IO (Texture Float)  #-}

data Triangle a = Triangle
  { tP0      :: Vec3 a
  , tP1      :: Vec3 a
  , tP2      :: Vec3 a
  , tTexture :: TextureDescription a
  } deriving (Read, Show, Eq, Ord)

mkTriangle :: (RealFloat a, Unbox a, Ord a, RealFrac a, Floating a) => Triangle a -> IO (TriangleData a)
mkTriangle (Triangle p0 p1 p2 tex) = do
  tex' <- mkTexture tex
  return (TriangleData
    { tdP0  = p0
    , tdP1  = p1
    , tdP2  = p2
    , tdTex = tex' })
{-# SPECIALIZE mkTriangle :: Triangle Double -> IO (TriangleData Double) #-}
{-# SPECIALIZE mkTriangle :: Triangle Float  -> IO (TriangleData Float)  #-}

data Sphere a = Sphere
  { sCenter  :: Vec3 a
  , sRadius  :: a
  , sTexture :: TextureDescription a
  } deriving (Read, Show, Eq, Ord)

mkSphere :: (RealFloat a, Unbox a, Ord a, RealFrac a, Floating a) => Sphere a -> IO (SphereData a)
mkSphere (Sphere c r tex) = do
  tex' <- mkTexture tex
  return (SphereData
    { sphereCenter = c
    , sphereRadius = r
    , sphereTex    = tex' })
{-# SPECIALIZE mkSphere :: Sphere Double -> IO (SphereData Double) #-}
{-# SPECIALIZE mkSphere :: Sphere Float  -> IO (SphereData Float)  #-}

data Plane a = Plane
  { pCenter  :: Vec3 a
  , pNormal  :: Vec3 a
  , pTexture :: TextureDescription a
  } deriving (Read, Show, Eq, Ord)

mkPlane :: (RealFloat a, Unbox a, Ord a, RealFrac a, Floating a) => Plane a -> IO (PlaneData a)
mkPlane (Plane c n tex) = do
  tex' <- mkTexture tex
  return (PlaneData
    { pdCenter  = c
    , pdNormal  = n
    , pdTex     = tex' })
{-# SPECIALIZE mkPlane :: Plane Double -> IO (PlaneData Double) #-}
{-# SPECIALIZE mkPlane :: Plane Float  -> IO (PlaneData Float)  #-}

mkShape :: SceneElement -> IO (Maybe (Shape RealTy))
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

mkShapes :: Scene -> IO [Shape RealTy]
mkShapes scene = catMaybes <$> mapM mkShape scene

readScene :: FilePath -> IO Scene
readScene fp = do
  cs <- S.readFile fp
  return (read cs)

mkCamera :: Scene -> Maybe (C.Camera RealTy, Int, Int)
mkCamera = listToMaybe . catMaybes . map mkCamera'
  where
  mkCamera' :: SceneElement -> Maybe (C.Camera RealTy, Int, Int)
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

readSceneToShapes :: FilePath -> IO [Shape RealTy]
readSceneToShapes fp = do
  sc <- readScene fp
  mkShapes sc

readSceneToCamera :: FilePath -> IO (Maybe (C.Camera RealTy, Int, Int))
readSceneToCamera fp = do
  sc <- readScene fp
  return (mkCamera sc)

mkDirectedLights :: Scene -> [DirectedLight RealTy]
mkDirectedLights = catMaybes . map mkDirectedLights'
  where
  mkDirectedLights' :: SceneElement -> Maybe (DirectedLight RealTy)
  mkDirectedLights' (SEDirectedLight l) = Just l 
  mkDirectedLights' _                   = Nothing

mkAmbientLight :: Scene -> Maybe (AmbientLight RealTy)
mkAmbientLight = listToMaybe . catMaybes . map mkAmbientLight'
  where
  mkAmbientLight' :: SceneElement -> Maybe (AmbientLight RealTy)
  mkAmbientLight' (SEAmbientLight l) = Just l 
  mkAmbientLight' _                  = Nothing

