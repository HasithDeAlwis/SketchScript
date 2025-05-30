module Parser.Widget (parseWidget) where

import Parser.Common (noIndents, parseContent, parseSizing, parseSpaceOrNothing)
import SketchScriptTypes
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import Types (Parser)

parseWidget :: Parser Widget
parseWidget = do
  _ <- TMC.char '<'
  partialWidget <-
    TM.try
      (Widget Button <$ TMC.string "button")
      TM.<|> (Widget InputBox <$ TMC.string "input")
      TM.<|> (Widget TextField <$ TMC.string "text")
      TM.<|> (Widget Image <$ TMC.string "image")

  spacingParser <- do
    hasQuote <- TM.lookAhead (TMC.char '"' >> pure True) TM.<|> pure False
    pure $ if hasQuote then TMC.space1 else parseSpaceOrNothing

  widget <-
    TM.try
      ( partialWidget
          <$> (TMC.space1 *> parseSizing <* spacingParser)
          <*> parseContent
      )
      TM.<|> pure (partialWidget (Sizing Nothing) (Content Nothing))
  _ <- TMC.space *> TMC.char '>' *> noIndents
  pure widget
