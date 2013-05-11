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
import Graphics.Rendering.Haray.HMDInfo
import Data.Vector.Unboxed (Unbox)

import Control.Monad
import Control.Monad.Primitive
import System.Random.MWC

type Scene a = [SceneElement a]

data SceneElement a
  = SESphere   (Sphere a)
  | SETriangle (Triangle a)
  | SEPlane    (Plane a)
  -- | TODO: fix me: never create ADT with record selectors. Partial functions suck.
  | SECamera { camCenter :: Vec3 a
             , camGaze   :: Vec3 a
             , camUp     :: Vec3 a
             , camDist   :: a
             , camNX     :: Int
             , camNY     :: Int
             , camFov    :: a
             , camLens   :: C.LensType a }
  | SEHMDInfo       (HMDInfo a)
  | SEDirectedLight (DirectedLight a)
  | SEAmbientLight  (AmbientLight a)
  deriving (Read, Show, Eq, Ord)

data TextureDescription a = Matte (RGB a)
  | Stripe
  | BWNoise
  | Marble !a
  deriving (Read, Show, Eq, Ord)

mkTexture :: (RealFloat a, Unbox a, Ord a, RealFrac a, Floating a, PrimMonad m)
          => Gen (PrimState m) -> TextureDescription a -> m (Texture a)
mkTexture _   (Matte rgb) = return (mkMatteTexture (MatteData rgb))
mkTexture _   Stripe      = return mkStripeTexture
mkTexture gen BWNoise     = mkBWNoiseTexture gen
mkTexture gen (Marble s)  = mkMarbleData gen s >>= (return . mkMarbleTexture)
{-# SPECIALIZE mkTexture :: (PrimMonad m) => Gen (PrimState m) -> TextureDescription Double -> m (Texture Double) #-}
{-# SPECIALIZE mkTexture :: (PrimMonad m) => Gen (PrimState m) -> TextureDescription Float  -> m (Texture Float)  #-}

data Triangle a = Triangle
  { tP0      :: Vec3 a
  , tP1      :: Vec3 a
  , tP2      :: Vec3 a
  , tTexture :: TextureDescription a
  } deriving (Read, Show, Eq, Ord)

mkTriangle :: (RealFloat a, Unbox a, Ord a, RealFrac a, Floating a, PrimMonad m)
           => Gen (PrimState m) -> Triangle a -> m (TriangleData a)
mkTriangle gen (Triangle p0 p1 p2 tex) = do
  tex' <- mkTexture gen tex
  return (TriangleData
    { tdP0  = p0
    , tdP1  = p1
    , tdP2  = p2
    , tdTex = tex' })
{-# SPECIALIZE mkTriangle :: (PrimMonad m) => Gen (PrimState m) -> Triangle Double -> m (TriangleData Double) #-}
{-# SPECIALIZE mkTriangle :: (PrimMonad m) => Gen (PrimState m) -> Triangle Float  -> m (TriangleData Float)  #-}

data Sphere a = Sphere
  { sCenter  :: Vec3 a
  , sRadius  :: a
  , sTexture :: TextureDescription a
  } deriving (Read, Show, Eq, Ord)

mkSphere :: (RealFloat a, Unbox a, Ord a, RealFrac a, Floating a, PrimMonad m)
         => Gen (PrimState m) -> Sphere a -> m (SphereData a)
mkSphere gen (Sphere c r tex) = do
  tex' <- mkTexture gen tex
  return (SphereData
    { sphereCenter = c
    , sphereRadius = r
    , sphereTex    = tex' })
{-# SPECIALIZE mkSphere :: (PrimMonad m) => Gen (PrimState m) -> Sphere Double -> m (SphereData Double) #-}
{-# SPECIALIZE mkSphere :: (PrimMonad m) => Gen (PrimState m) -> Sphere Float  -> m (SphereData Float)  #-}

data Plane a = Plane
  { pCenter  :: Vec3 a
  , pNormal  :: Vec3 a
  , pTexture :: TextureDescription a
  } deriving (Read, Show, Eq, Ord)

mkPlane :: (RealFloat a, Unbox a, Ord a, RealFrac a, Floating a, PrimMonad m)
        => Gen (PrimState m) -> Plane a -> m (PlaneData a)
mkPlane gen (Plane c n tex) = do
  tex' <- mkTexture gen tex
  return (PlaneData
    { pdCenter  = c
    , pdNormal  = n
    , pdTex     = tex' })
{-# SPECIALIZE mkPlane :: (PrimMonad m) => Gen (PrimState m) -> Plane Double -> m (PlaneData Double) #-}
{-# SPECIALIZE mkPlane :: (PrimMonad m) => Gen (PrimState m) -> Plane Float  -> m (PlaneData Float)  #-}

mkShape :: (RealFrac a, RealFloat a, Unbox a, PrimMonad m)
        => Gen (PrimState m) -> SceneElement a -> m (Maybe (Shape a))
mkShape gen (SESphere sd)   = do
  s <- mkSphere gen sd
  return $ Just $ Shape.mkSphere s
mkShape gen (SETriangle td) = do
  t <- mkTriangle gen td
  return $ Just $ Shape.mkTriangle t
mkShape gen (SEPlane pd)    = do
  p <- mkPlane gen pd
  return $ Just $ Shape.mkPlane p
mkShape _   _               = return Nothing
{-# SPECIALIZE mkShape :: (PrimMonad m) => Gen (PrimState m) -> SceneElement Double -> m (Maybe (Shape Double)) #-}
{-# SPECIALIZE mkShape :: (PrimMonad m) => Gen (PrimState m) -> SceneElement Float  -> m (Maybe (Shape Float)) #-}

mkShapes :: (Unbox a, RealFrac a, RealFloat a, PrimMonad m)
         => Gen (PrimState m) -> Scene a -> m [Shape a]
mkShapes gen scene = catMaybes `liftM` mapM (mkShape gen) scene
{-# SPECIALIZE mkShapes :: (PrimMonad m) => Gen (PrimState m) -> Scene Double -> m [Shape Double] #-}
{-# SPECIALIZE mkShapes :: (PrimMonad m) => Gen (PrimState m) -> Scene Float  -> m [Shape Float]  #-}

readScene :: Read a => FilePath -> IO (Scene a)
readScene fp = do
  cs <- S.readFile fp
  return (read cs)

mkCamera :: (Floating a, Ord a) => Scene a -> Maybe (C.Camera a)
mkCamera = listToMaybe . catMaybes . map mkCamera'
  where
  mkCamera' c@(SECamera{}) = Just (C.mkCamera (camLens c)
                                              (camCenter c)
                                              (camGaze c)
                                              (camUp c)
                                              (camDist c)
                                              (camFov c)
                                              (camNX c)
                                              (camNY c))
  mkCamera' _              = Nothing
{-# SPECIALIZE mkCamera :: Scene Double -> Maybe (C.Camera Double) #-}
{-# SPECIALIZE mkCamera :: Scene Float  -> Maybe (C.Camera Float)  #-}

mkHMDInfo :: Scene a -> Maybe (HMDInfo a)
mkHMDInfo = listToMaybe . catMaybes . map mkHMDInfo'
  where
  mkHMDInfo' :: SceneElement a -> Maybe (HMDInfo a)
  mkHMDInfo' (SEHMDInfo hmdi) = Just hmdi
  mkHMDInfo' _                = Nothing

readSceneToShapes :: (Unbox a, RealFrac a, RealFloat a, Read a)
                  => GenST RealWorld -> FilePath -> IO [Shape a]
readSceneToShapes gen fp = do
  sc <- readScene fp
  mkShapes gen sc

readSceneToCamera :: (Read a, Floating a, Ord a)
                  => FilePath -> IO (Maybe (C.Camera a))
readSceneToCamera fp = do
  sc <- readScene fp
  return (mkCamera sc)

mkDirectedLights :: Scene a -> [DirectedLight a]
mkDirectedLights = catMaybes . map mkDirectedLights'
  where
  mkDirectedLights' :: SceneElement a -> Maybe (DirectedLight a)
  mkDirectedLights' (SEDirectedLight l) = Just l 
  mkDirectedLights' _                   = Nothing

mkAmbientLight :: Scene a -> Maybe (AmbientLight a)
mkAmbientLight = listToMaybe . catMaybes . map mkAmbientLight'
  where
  mkAmbientLight' :: SceneElement a -> Maybe (AmbientLight a)
  mkAmbientLight' (SEAmbientLight l) = Just l 
  mkAmbientLight' _                  = Nothing

