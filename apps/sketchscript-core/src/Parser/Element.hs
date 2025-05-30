module Parser.Element (parseElement, parseElements) where

import Parser.Common (parseSizing, parseSpaceOrNothing)
import Parser.Style (parseStyle)
import Parser.Widget (parseWidget)
import SketchScriptTypes (Container (..), ContainerType (..), Element (..))
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Megaparsec.Char.Lexer as TMCL
import Text.Megaparsec.Pos (mkPos, unPos)
import Types (Parser)

parseElements :: Int -> Parser [Element]
parseElements currentIndent = TM.some $ TM.try (parseElement currentIndent)

parseElement :: Int -> Parser Element
parseElement currentIndent =
  TMCL.indentGuard TMC.space EQ (mkPos currentIndent)
    *> ( TM.try (ElementContainer <$> parseContainer currentIndent)
           TM.<|> (ElementWidget <$> parseWidget)
       )
    <* (TMC.space *> TM.optional TMC.newline)

parseContainer :: Int -> Parser Container
parseContainer parentIndent = do
  _ <- TMC.char '<'
  _ <- TMC.string "Box" *> (TM.try TMC.space1 TM.<|> parseSpaceOrNothing)
  size <- parseSizing <* (TM.try TMC.space1 TM.<|> parseSpaceOrNothing)
  style <- parseStyle <* parseSpaceOrNothing
  _ <- TMC.char '>'

  children <-
    TM.try
      ( do
          _ <- TMC.newline
          deeperIndent <- TMCL.indentGuard TMC.space GT (mkPos parentIndent)
          parseElements (unPos deeperIndent)
      )
      TM.<|> pure []

  pure $ Container Box size style children