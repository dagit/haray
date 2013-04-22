module Main where

import Graphics.Rendering.Haray.Render ( renderSceneFromTo )
import System.Environment ( getArgs )
import Control.Monad ( when )

main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) (error "Usage: haray <input.scene> <output.png>")
  let outfile  = head (drop 1 args)
      input    = head args
  renderSceneFromTo input outfile
