module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Parser.Script (parseSketchScript)

main :: IO ()
main = do
  putStrLn "Enter your script:"
  input <- TIO.getContents
  print $ parseSketchScript (T.unpack input)