module Data.Bitmap
( BMP
, allocBMP 
, pokePixel
, writeBMP
) where

import qualified Codec.BMP as BMP
import qualified Data.ByteString.Internal as BI
import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.Storable

type RGB = (Word8, Word8, Word8)

rgbaSize :: Int
rgbaSize = 4

data BMP = BMP
  { bmpChunk  :: !(ForeignPtr Word8)
  , bmpWidth  :: !Int
  , bmpHeight :: !Int }

allocBMP :: Int -> Int -> IO BMP
allocBMP width height = do
  mem <- mallocForeignPtrBytes sz
  return $! BMP mem width height
  where sz = rgbaSize * width * height

{-# INLINE pokePixel #-}
pokePixel :: Int -> Int -> RGB -> BMP -> IO ()
pokePixel x y _ (BMP chnk w h)
  | x < 0 || x > w || y < 0 || y > h = error "pokePixel: out of bounds"
pokePixel x y rgb bmp = unsafePokePixel x y rgb bmp

{-# INLINE unsafePokePixel #-}
unsafePokePixel :: Int -> Int -> RGB -> BMP -> IO ()
unsafePokePixel x y (r,g,b) (BMP mem w _) = do
  let off = x*rgbaSize + y*w*rgbaSize
  withForeignPtr mem $ \ptr -> do
    pokeElemOff ptr off r
    pokeElemOff ptr (off+1) g
    pokeElemOff ptr (off+2) b
    pokeElemOff ptr (off+3) 0

{-# INLINE bmpToBMP #-}
bmpToBMP :: BMP -> BMP.BMP
bmpToBMP (BMP rgba w h) = BMP.packRGBA32ToBMP w h bs
  where bs = BI.fromForeignPtr rgba 0 (w*h*rgbaSize)

{-# INLINE writeBMP #-}
writeBMP :: FilePath -> BMP -> IO ()
writeBMP fp bmp = BMP.writeBMP fp (bmpToBMP bmp)
