module Data.Bitmap
( BMP(..)
, allocBMP 
, pokePixel
) where

import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.Storable

type RGB = (Word8, Word8, Word8)

rgbaSize :: Int
rgbaSize = 3

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
pokePixel x y rgb bmp@(BMP _ _ h) = unsafePokePixel x (h - y + 1) rgb bmp

{-# INLINE unsafePokePixel #-}
unsafePokePixel :: Int -> Int -> RGB -> BMP -> IO ()
unsafePokePixel x y (r,g,b) (BMP mem w _) = do
  let off = x*rgbaSize + y*w*rgbaSize
  withForeignPtr mem $ \ptr -> do
    pokeElemOff ptr off r
    pokeElemOff ptr (off+1) g
    pokeElemOff ptr (off+2) b
