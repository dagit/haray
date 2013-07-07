module Main where

import Graphics.Rendering.Haray.Render ( renderSceneFromTo, renderDefinition )
import System.Environment ( getArgs )
import Control.Monad ( when )

#ifdef USE_OPENCL
import Language.C.Syntax
import Language.C.Quote.OpenCL
import Text.PrettyPrint.Mainland
import Control.Parallel.OpenCL
import Foreign( castPtr, nullPtr, sizeOf )
import Foreign.C.Types( CFloat, CInt )
import Foreign.Marshal.Array( newArray, peekArray )
import Graphics.Rendering.Haray.RGB
import Control.Monad
import Graphics.Rendering.Haray.Bitmap
import Codec.Picture.Types ( Image(..), unsafeFreezeImage, PixelRGB8(..), withImage )
import Codec.Picture ( writePng, writePixel, pixelAt )
import Control.Applicative
import qualified Data.Vector as V
#endif


main :: IO ()
#ifndef USE_OPENCL
main = do
  args <- getArgs
  when (length args < 2) (error "Usage: haray <input.scene> <output.png>")
  let outfile  = head (drop 1 args)
      input    = head args
  renderSceneFromTo input outfile
#else
main = do
  let programSource = show (ppr renderDefinition)
      (nx,ny)       = (500,500) :: (CInt, CInt)
  -- Initialize OpenCL
  (platform:_) <- clGetPlatformIDs
  (dev:_) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- clCreateContext [CL_CONTEXT_PLATFORM platform] [dev] print
  q <- clCreateCommandQueue context dev []
  
  -- Initialize Kernel
  program <- clCreateProgramWithSource context programSource
  clBuildProgram program [dev] ""
  kernel <- clCreateKernel program "renderScene"
  
  -- Initialize parameters
  let original = [0 .. fromIntegral (nx*ny*3)] :: [CFloat]
      elemSize = sizeOf (0 :: CFloat)
      vecSize  = elemSize * length original
  input  <- newArray original

  mem_in <- clCreateBuffer context [CL_MEM_READ_WRITE
                                   ,CL_MEM_COPY_HOST_PTR]
                                   (vecSize, castPtr input)  

  clSetKernelArgSto kernel 0 mem_in
  clSetKernelArgSto kernel 1 nx
  clSetKernelArgSto kernel 2 ny
  
  -- Execute Kernel
  eventExec <- clEnqueueNDRangeKernel q kernel [nx*ny] [1] []
  
  -- Get Result
  eventRead <- clEnqueueReadBuffer q mem_in True 0 vecSize (castPtr input)
                                                            [eventExec]
  
  result <- V.fromList <$> peekArray (length original) input
  let nx' = fromIntegral nx :: Int
      ny' = fromIntegral ny :: Int
  img <- mkMutableImage nx' ny'
  forM_ [0.. ny'-1] $ \y ->
    forM_ [0.. nx'-1] $ \x -> do
      let idx1 = 3 * (x + y * ny') :: Int
          idx2 = idx1 + 1
          idx3 = idx2 + 1
      writePixel img x y
        (PixelRGB8 (toWord8 (clamp (result V.! idx1)))
                   (toWord8 (clamp (result V.! idx2)))
                   (toWord8 (clamp (result V.! idx3))))

  img' <- unsafeFreezeImage img
  writePng "test.png" img'

#endif
