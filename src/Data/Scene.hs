module Data.Scene where

import Data.Maybe
import Data.Shape
import qualified System.IO.Strict as S
import qualified Data.Camera as C
import Data.VectorSpace

type Scene = [SceneElement]

data SceneElement = SESphere (SphereData Float)
                  | SETriangle (TriangleData Float)
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
  deriving (Read, Show, Eq, Ord)


mkShape :: SceneElement -> Maybe (Shape Float)
mkShape (SESphere sd)   = Just $ mkSphere sd
mkShape (SETriangle td) = Just $ mkTriangle td
mkShape _               = Nothing

mkShapes :: Scene -> [Shape Float]
mkShapes = catMaybes . map mkShape

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
  return (mkShapes sc)

readSceneToCamera :: FilePath -> IO (Maybe (C.Camera Float, Int, Int))
readSceneToCamera fp = do
  sc <- readScene fp
  return (mkCamera sc)
