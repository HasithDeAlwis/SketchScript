module CodeGenerators.Widget where

import CodeGenerators.Utils
import Data.Maybe (fromMaybe)
import SketchScriptTypes

genWidget :: Widget -> String
genWidget (Widget widgetType sizing content) =
  genWidgetType widgetType content (genTailwindClasses widgetType sizing (Flex Start))

genWidgetType :: WidgetType -> Content -> String -> String
genWidgetType Button (Content mText) classes =
  "<button class=\"" ++ classes ++ "\">" ++ fromMaybe "Button" mText ++ "</button>"
genWidgetType InputBox (Content mText) classes =
  "<input disabled type=\"text\" class=\"" ++ classes ++ "\" placeholder=\"" ++ fromMaybe "" mText ++ "\" />"
genWidgetType TextField (Content mText) classes =
  "<p class=\"" ++ classes ++ "\">" ++ fromMaybe "Text" mText ++ "</p>"
genWidgetType Image (Content _mUrl) classes =
  "<img alt=\"Image\" class=\"" ++ classes ++ "\" />"
