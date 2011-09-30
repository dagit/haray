module Data.Scene where

import Data.Shape
import qualified System.IO.Strict as S

type Scene = [SceneElement]

data SceneElement = SESphere (SphereData Float)
                  | SETriangle (TriangleData Float)
  deriving (Read, Show, Eq, Ord)


mkShape :: SceneElement -> Shape Float
mkShape (SESphere sd)   = mkSphere sd
mkShape (SETriangle td) = mkTriangle td

mkShapes :: Scene -> [Shape Float]
mkShapes = map mkShape

readScene :: FilePath -> IO Scene
readScene fp = do
  cs <- S.readFile fp
  return (read cs)

readSceneToShapes :: FilePath -> IO [Shape Float]
readSceneToShapes fp = do
  sc <- readScene fp
  return (mkShapes sc)
