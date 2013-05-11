{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Rendering.Haray.Bitmap where

import Codec.Picture.Types
import Data.Vector.Storable.Mutable as M
import Control.Monad.Primitive ( PrimMonad, PrimState )

mkMutableImage :: forall p m. (Pixel p, PrimMonad m) => Int -> Int -> m (MutableImage (PrimState m) p)
mkMutableImage width height = do
  let compCount = componentCount (undefined :: p)
  v <- M.replicate (width * height * compCount) 0
  return $! MutableImage { mutableImageWidth  = width
                         , mutableImageHeight = height
                         , mutableImageData   = v }
