module Main where

#ifndef USE_OPENCL

import Graphics.Rendering.Haray.Render ( renderSceneFromTo )
import System.Environment ( getArgs )
import Control.Monad ( when )

#else

import Codec.Picture ( writePng, writePixel )
import Codec.Picture.Types ( unsafeFreezeImage, PixelRGB8(..) )
import Control.Applicative
import Control.Monad
import Control.Parallel.OpenCL
import Foreign( castPtr, sizeOf )
import Foreign.C.Types( CFloat, CInt )
import Foreign.Marshal.Array( newArray, peekArray )
import Graphics.Rendering.Haray.Bitmap
import Graphics.Rendering.Haray.RGB
import Graphics.Rendering.Haray.Render ( renderDefinition )
import Text.PrettyPrint.Mainland
import qualified Data.Time as T
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
  start <- T.getCurrentTime
  let getTime = fromRational . toRational . flip T.diffUTCTime start <$> T.getCurrentTime :: IO Double
      programSource = show (ppr renderDefinition)
      (nx,ny)       = (500,500) :: (CInt, CInt)
  -- Initialize OpenCL
  (platform:_) <- clGetPlatformIDs
  (dev:_) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- clCreateContext [CL_CONTEXT_PLATFORM platform] [dev] print
  q <- clCreateCommandQueue context dev []
  
  -- Initialize Kernel
  t0 <- getTime
  program <- clCreateProgramWithSource context programSource
  clBuildProgram program [dev] ""
  t1 <- getTime
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
  t2 <- getTime
  eventExec <- clEnqueueNDRangeKernel q kernel [nx*ny] [1] []
  t3 <- getTime
  
  -- Get Result
  _ <- clEnqueueReadBuffer q mem_in True 0 vecSize (castPtr input)
                                                   [eventExec]
  t4 <- getTime
  
  result <- V.fromList <$> peekArray (length original) input
  t5 <- getTime
  -- release memory / device handles when done
  void (clReleaseKernel kernel)
  void (clReleaseProgram program)
  void (clReleaseMemObject mem_in)
  void (clReleaseCommandQueue q)
  void (clReleaseContext context)
  t6 <- getTime
  -- Write the output image
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
  t7 <- getTime
  putStrLn ("Compilation time:     " ++ show (t1 - t0))
  putStrLn ("Parameter setup time: " ++ show (t2 - t1))
  putStrLn ("Execution time:       " ++ show (t3 - t2))
  putStrLn ("Fetch result time:    " ++ show (t4 - t3))
  putStrLn ("Convert to vector:    " ++ show (t5 - t4))
  putStrLn ("Release resources:    " ++ show (t6 - t5))
  putStrLn ("Write png:            " ++ show (t7 - t6))


#endif
