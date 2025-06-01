module CodeGenerators.Utils (genTailwindClasses, genTailwindContainerClasses) where

import Data.Maybe (fromMaybe)
import SketchScriptTypes

genSizing :: Sizing -> String
genSizing (Sizing x) =
  let percent = fromMaybe 1 x * 100
   in "w-[" ++ show percent ++ "%]"

genJustify :: Justify -> String
genJustify Start = "justify-start"
genJustify Center = "justify-center"
genJustify End = "justify-end"
genJustify SpaceBetween = "justify-between"
genJustify SpaceAround = "justify-around"

genLayoutStyle :: Style -> String
genLayoutStyle (Flex j) = "flex " ++ genJustify j
genLayoutStyle (Grid (ColsAndRows c r)) =
  "grid grid-cols-" ++ show c ++ " grid-rows-" ++ show r

genWidgetBaseStyle :: WidgetType -> String
genWidgetBaseStyle Button =
  "bg-red-400 px-4 py-2 rounded text-center"
genWidgetBaseStyle InputBox = "px-2 py-1 bg-blue-200"
genWidgetBaseStyle TextField = "p-2 text-gray-200"
genWidgetBaseStyle Image = "object-contain"

genPlaceholderIfEmpty :: WidgetType -> String
genPlaceholderIfEmpty Image = "bg-gray-100"
genPlaceholderIfEmpty _ = "bg-gray-100 text-gray-500"

genTailwindClasses :: WidgetType -> Sizing -> Style -> String
genTailwindClasses widgetType sizing style =
  unwords
    [ genSizing sizing,
      genLayoutStyle style,
      genWidgetBaseStyle widgetType,
      "border border-black",
      genPlaceholderIfEmpty widgetType
    ]

genTailwindContainerClasses :: Sizing -> Style -> [Element] -> String
genTailwindContainerClasses sizing style children =
  unwords
    [ genSizing sizing,
      genLayoutStyle style,
      "border border-black",
      if null children then "bg-gray-100" else ""
    ]
