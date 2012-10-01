{-
  Adapted from here: http://www.haskell.org/haskellwiki/Library/PNG
-}
module Data.PNG (png, writePNG) where

import Data.Bitmap
import Data.Array
import Data.Bits
import Data.List
import Data.Word
import Data.Int
import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Internal as BI

import Foreign.ForeignPtr
 
be8 :: Word8 -> B.ByteString
be8 x = B.singleton x
 
be32 :: Word32 -> B.ByteString
be32 x = B.pack [fromIntegral (x `shiftR` sh) | sh <- [24,16,8,0]]
 
pack :: String -> B.ByteString
pack xs = B.pack $ map (fromIntegral.fromEnum) xs
 
unpack :: B.ByteString -> String
unpack xs = map (toEnum.fromIntegral) (B.unpack xs)
 
hdr, iHDR, iDAT, iEND :: B.ByteString
hdr = pack "\137\80\78\71\13\10\26\10"
iHDR = pack "IHDR"
iDAT = pack "IDAT"
iEND = pack "IEND"
 
chunk :: B.ByteString -> B.ByteString -> [B.ByteString]
chunk tag xs = [be32 (fromIntegral $ B.length xs), dat, be32 (crc dat)]
    where dat = B.append tag xs
 
png :: BMP -> B.ByteString
png dat = B.concat $ hdr : concat [ihdr, imgdat ,iend]
     where height = fromIntegral $ bmpHeight dat :: Word32
           width = fromIntegral $ bmpWidth dat :: Word32
           len = fromIntegral $ height * width * 3
           bs = B.fromChunks [BI.fromForeignPtr (bmpChunk dat) 0 len]
           lines = split (3*fromIntegral width) bs
           ihdr = chunk iHDR $ B.concat 
                     [ be32 height
                     , be32 width
                     , be8 8   -- bits per sample (8 for r, 8 for g, 8 for b)
                     , be8 2   -- color type (2=rgb)
                     , be8 0   -- compression method
                     , be8 0   -- filter method
                     , be8 0 ] -- interlace method
           imgdat = chunk iDAT (Z.compress imagedata)
           imagedata = B.concat $ map scanline lines
           iend = chunk iEND B.empty

writePNG :: FilePath -> BMP -> IO ()
writePNG fp bmp = B.writeFile fp (png bmp)

split :: Int64 -> B.ByteString -> [B.ByteString]
split n bs
  | B.length bs > n = case B.splitAt n bs of
                        (hs, ts) -> hs : split n ts
  | otherwise = [bs]
 
scanline :: B.ByteString -> B.ByteString
scanline dat = 0 `B.cons` dat

crc :: B.ByteString -> Word32
crc xs = updateCrc 0xffffffff xs `xor` 0xffffffff
 
updateCrc :: Word32 -> B.ByteString -> Word32
updateCrc = B.foldl' crcStep
 
crcStep :: Word32 -> Word8 -> Word32
crcStep crc ch = (crcTab ! n) `xor` (crc `shiftR` 8)
    where n = fromIntegral (crc `xor` fromIntegral ch)
 
crcTab :: Array Word8 Word32
crcTab = listArray (0,255) $ flip map [0..255] (\n ->
    foldl' (\c k -> if c .&. 1 == 1
                      then 0xedb88320 `xor` (c `shiftR` 1)
                      else c `shiftR` 1) n [0..7])
