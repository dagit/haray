module Graphics.Rendering.Haray.Bitmap where

import Codec.Picture.Types
import Codec.Picture.Png
import Data.Vector.Storable.Mutable
import Control.Monad.ST
import Data.Vector.Storable ( freeze )

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

writePngRGB8 :: FilePath -> MutableImage RealWorld PixelRGB8 -> IO ()
writePngRGB8 outfile bmp = do
  imageD <- freeze (mutableImageData bmp)
  writePng outfile (Image { imageWidth  = mutableImageWidth bmp
                          , imageHeight = mutableImageHeight bmp
                          , imageData   = imageD
                          } :: Image PixelRGB8)

