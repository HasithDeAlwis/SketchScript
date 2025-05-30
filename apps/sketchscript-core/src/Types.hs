module Types where

import Data.Void (Void)
import qualified Text.Megaparsec as TM

type Parser = TM.Parsec Void String