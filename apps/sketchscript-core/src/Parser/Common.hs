module Parser.Common
  ( parseSizing,
    parseContent,
    parseQuotationMarks,
    parseSpaceOrNothing,
    noIndents,
  )
where

import qualified Control.Monad
import SketchScriptTypes
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Megaparsec.Char.Lexer as TMCL
import Types

parseSizing :: Parser Sizing
parseSizing =
  TM.try
    (Sizing . Just <$> TMCL.float)
    TM.<|> (Sizing . Just . fromIntegral <$> TMCL.decimal)
    TM.<|> pure (Sizing Nothing)

parseQuotationMarks :: Parser String
parseQuotationMarks = do
  TM.try $ TMC.char '"' *> TM.manyTill TMCL.charLiteral (TMC.char '"')

parseContent :: Parser Content
parseContent = do
  TM.try (Content <$> fmap Just parseQuotationMarks) TM.<|> pure (Content Nothing)

parseSpaceOrNothing :: Parser ()
parseSpaceOrNothing = TM.try TMC.space1 TM.<|> pure ()

noIndents :: Parser ()
noIndents = do
  currentIndent <- TMCL.indentLevel
  next <- TM.lookAhead TMCL.indentLevel
  Control.Monad.when (next > currentIndent) $ fail "Widgets cannot be followed by indentation"
