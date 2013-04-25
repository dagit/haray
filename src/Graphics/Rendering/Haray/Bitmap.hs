module Graphics.Rendering.Haray.Bitmap where

import Codec.Picture.Types
import Data.Vector.Storable.Mutable
import Control.Monad.ST

mkImage :: Int -> Int -> ST s (MutableImage s PixelRGB8)
mkImage width height = do
  let compCount = componentCount (undefined :: PixelRGB8)
  v <- new (width * height * compCount)
  return $! MutableImage { mutableImageWidth  = width
                         , mutableImageHeight = height
                         , mutableImageData   = v }

writePixelRGBIO :: MutableImage RealWorld PixelRGB8 -> Int -> Int -> PixelRGB8 -> IO ()
writePixelRGBIO bmp x y p = stToIO (writePixel bmp x (h - y - 1) p)
  where
  h = mutableImageHeight bmp

writePixelRGB :: MutableImage s PixelRGB8 -> Int -> Int -> PixelRGB8 -> ST s ()
writePixelRGB bmp x y p = writePixel bmp x (h - y - 1) p
  where
  h = mutableImageHeight bmp
