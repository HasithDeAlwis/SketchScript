module Parser.Script (parseSketchScript) where

import Parser.Element (parseElements)
import SketchScriptTypes (Element)
import Text.Megaparsec (errorBundlePretty, parse)
import qualified Text.Megaparsec.Char as TMC
import Types (Parser)

parseScript :: Parser [Element]
parseScript = do
  TMC.space
  parseElements 1

parseSketchScript :: String -> Either String [Element]
parseSketchScript input =
  case parse parseScript "sketch-script" input of
    Left err -> Left (errorBundlePretty err)
    Right ast -> Right ast