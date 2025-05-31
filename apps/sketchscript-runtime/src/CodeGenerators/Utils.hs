module CodeGenerators.Utils (genCSS) where

import Data.Maybe (fromMaybe)
import SketchScriptTypes

genSizing :: Sizing -> String
genSizing (Sizing x) = "width: " ++ show (fromMaybe 1 x * 100) ++ "%;"

genStyles :: Style -> String
genStyles (Flex justify) =
  "display: flex; justify-content: " ++ genJustify justify ++ ";"
genStyles (Grid (ColsAndRows c r)) =
  "display: grid; grid-template-columns: repeat(" ++ show c ++ ", 1fr); grid-template-rows: repeat(" ++ show r ++ ", 1fr);"

genJustify :: Justify -> String
genJustify Start = "flex-start"
genJustify Center = "center"
genJustify End = "flex-end"
genJustify SpaceBetween = "space-between"
genJustify SpaceAround = "space-around"

genCSS :: Sizing -> Style -> String
genCSS sizing styles =
  "style=\"" ++ genSizing sizing ++ " " ++ genStyles styles ++ "\""
