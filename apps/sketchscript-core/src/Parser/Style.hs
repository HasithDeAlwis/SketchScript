module Parser.Style (parseStyle, parseFlex, parseGrid, parseJustify) where

import Parser.Common (parseSpaceOrNothing)
import SketchScriptTypes
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Megaparsec.Char.Lexer as TMCL
import Types (Parser)

parseStyle :: Parser Style
parseStyle = do
  TM.try parseFlex TM.<|> parseGrid TM.<|> pure (Flex Start)

parseFlex :: Parser Style
parseFlex = do
  _ <- TMC.string "flex" *> parseSpaceOrNothing
  Flex <$> parseJustify

parseJustify :: Parser Justify
parseJustify =
  (Start <$ TMC.string "start")
    TM.<|> (Center <$ TMC.string "center")
    TM.<|> (End <$ TMC.string "end")
    TM.<|> (SpaceBetween <$ TMC.string "between")
    TM.<|> (SpaceAround <$ TMC.string "around")
    TM.<|> pure Start -- default value

parseGrid :: Parser Style
parseGrid = do
  _ <- TMC.string "grid" *> parseSpaceOrNothing
  cols' <- TM.try (TMCL.decimal <* parseSpaceOrNothing) TM.<|> pure 2
  rows' <- TM.try TMCL.decimal TM.<|> pure 2
  pure $ Grid $ ColsAndRows {cols = cols', rows = rows'}