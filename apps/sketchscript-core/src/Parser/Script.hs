module Parser.Script (parseScript) where

import Parser.Element (parseElements)
import SketchScriptTypes (Element)
import qualified Text.Megaparsec.Char as TMC
import Types (Parser)

parseScript :: Parser [Element]
parseScript = do
  TMC.space
  parseElements 1
