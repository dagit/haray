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
import Control.Applicative
import Data.Vector.Unboxed (Unbox)

import Control.Monad.ST
import System.Random.MWC

type Scene = [SceneElement]

type RealTy = Float


data SceneElement = SESphere (Sphere RealTy)
                  | SETriangle (Triangle RealTy)
                  | SEPlane (Plane RealTy)
                  -- | TODO: fix me: never create ADT with record selectors. Partial functions suck.
                  | SECamera { camCenter :: Vec3 RealTy
                             , camGaze   :: Vec3 RealTy
                             , camUp     :: Vec3 RealTy
                             , camDist   :: RealTy
                             , camNX     :: Int
                             , camNY     :: Int
                             , camFov    :: RealTy
                             , camLens   :: C.LensType RealTy }
                  -- | TODO: fix me: never create ADT with record selectors. Partial functions suck.
                  | SEHMDInfo (HMDInfo RealTy)
                  | SEDirectedLight (DirectedLight RealTy)
                  | SEAmbientLight (AmbientLight RealTy)
  deriving (Read, Show, Eq, Ord)

data TextureDescription a = Matte (RGB a)
  | Stripe
  | BWNoise
  | Marble !a
  deriving (Read, Show, Eq, Ord)

mkTexture :: (RealFloat a, Unbox a, Ord a, RealFrac a, Floating a) => GenST s -> TextureDescription a -> ST s (Texture a)
mkTexture _   (Matte rgb) = return (mkMatteTexture (MatteData rgb))
mkTexture _   Stripe      = return mkStripeTexture
mkTexture gen BWNoise     = mkBWNoiseTexture gen
mkTexture gen (Marble s)  = mkMarbleData gen s >>= (return . mkMarbleTexture)
{-# SPECIALIZE mkTexture :: GenST s -> TextureDescription Double -> ST s (Texture Double) #-}
{-# SPECIALIZE mkTexture :: GenST s -> TextureDescription Float  -> ST s (Texture Float)  #-}

data Triangle a = Triangle
  { tP0      :: Vec3 a
  , tP1      :: Vec3 a
  , tP2      :: Vec3 a
  , tTexture :: TextureDescription a
  } deriving (Read, Show, Eq, Ord)

mkTriangle :: (RealFloat a, Unbox a, Ord a, RealFrac a, Floating a) => GenST s -> Triangle a -> ST s (TriangleData a)
mkTriangle gen (Triangle p0 p1 p2 tex) = do
  tex' <- mkTexture gen tex
  return (TriangleData
    { tdP0  = p0
    , tdP1  = p1
    , tdP2  = p2
    , tdTex = tex' })
{-# SPECIALIZE mkTriangle :: GenST s -> Triangle Double -> ST s (TriangleData Double) #-}
{-# SPECIALIZE mkTriangle :: GenST s -> Triangle Float  -> ST s (TriangleData Float)  #-}

data Sphere a = Sphere
  { sCenter  :: Vec3 a
  , sRadius  :: a
  , sTexture :: TextureDescription a
  } deriving (Read, Show, Eq, Ord)

mkSphere :: (RealFloat a, Unbox a, Ord a, RealFrac a, Floating a) => GenST s -> Sphere a -> ST s (SphereData a)
mkSphere gen (Sphere c r tex) = do
  tex' <- mkTexture gen tex
  return (SphereData
    { sphereCenter = c
    , sphereRadius = r
    , sphereTex    = tex' })
{-# SPECIALIZE mkSphere :: GenST s -> Sphere Double -> ST s (SphereData Double) #-}
{-# SPECIALIZE mkSphere :: GenST s -> Sphere Float  -> ST s (SphereData Float)  #-}

data Plane a = Plane
  { pCenter  :: Vec3 a
  , pNormal  :: Vec3 a
  , pTexture :: TextureDescription a
  } deriving (Read, Show, Eq, Ord)

mkPlane :: (RealFloat a, Unbox a, Ord a, RealFrac a, Floating a) => GenST s -> Plane a -> ST s (PlaneData a)
mkPlane gen (Plane c n tex) = do
  tex' <- mkTexture gen tex
  return (PlaneData
    { pdCenter  = c
    , pdNormal  = n
    , pdTex     = tex' })
{-# SPECIALIZE mkPlane :: GenST s -> Plane Double -> ST s (PlaneData Double) #-}
{-# SPECIALIZE mkPlane :: GenST s -> Plane Float  -> ST s (PlaneData Float)  #-}

mkShape :: GenST s -> SceneElement -> ST s (Maybe (Shape RealTy))
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

mkShapes :: GenST s -> Scene -> ST s [Shape RealTy]
mkShapes gen scene = catMaybes <$> mapM (mkShape gen) scene

readScene :: FilePath -> IO Scene
readScene fp = do
  cs <- S.readFile fp
  return (read cs)

mkCamera :: Scene -> Maybe (C.Camera RealTy)
mkCamera = listToMaybe . catMaybes . map mkCamera'
  where
  mkCamera' :: SceneElement -> Maybe (C.Camera RealTy)
  mkCamera' c@(SECamera{}) = Just (C.mkCamera (camLens c)
                                              (camCenter c)
                                              (camGaze c)
                                              (camUp c)
                                              (camDist c)
                                              (camFov c)
                                              (camNX c)
                                              (camNY c))
  mkCamera' _              = Nothing

mkHMDInfo :: Scene -> Maybe (HMDInfo RealTy)
mkHMDInfo = listToMaybe . catMaybes . map mkHMDInfo'
  where
  mkHMDInfo' :: SceneElement -> Maybe (HMDInfo RealTy)
  mkHMDInfo' (SEHMDInfo hmdi) = Just hmdi
  mkHMDInfo' _                = Nothing

readSceneToShapes :: GenST RealWorld -> FilePath -> IO [Shape RealTy]
readSceneToShapes gen fp = do
  sc <- readScene fp
  stToIO (mkShapes gen sc)

readSceneToCamera :: FilePath -> IO (Maybe (C.Camera RealTy))
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

