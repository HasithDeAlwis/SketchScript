module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Parser.Script (parseScript)
import Text.Megaparsec
import Types (Parser)

main :: IO ()
main = do
  putStrLn "Enter your script:"
  input <- TIO.getContents
  case parse parseScript "<stdin>" (T.unpack input) of
    Left err -> putStrLn (errorBundlePretty err)
    Right ast -> print ast
