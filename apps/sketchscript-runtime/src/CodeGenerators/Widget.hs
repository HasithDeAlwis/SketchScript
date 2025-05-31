module CodeGenerators.Widget where

import CodeGenerators.Utils
import Data.Maybe (fromMaybe)
import SketchScriptTypes

genWidget :: Widget -> String
genWidget (Widget widgetType sizing content) =
  genWidgetType widgetType content (genCSS sizing (Flex Start))

genWidgetType :: WidgetType -> Content -> String -> String
genWidgetType Button (Content mText) style =
  "<button " ++ style ++ ">" ++ fromMaybe "" mText ++ "</button>"
genWidgetType InputBox (Content _) style =
  "<input type=\"text\" " ++ style ++ " />"
genWidgetType TextField (Content mText) style =
  "<p " ++ style ++ ">" ++ fromMaybe "" mText ++ "</p>"
genWidgetType Image (Content _mUrl) style =
  -- url is not used here
  "<img alt=\"Image\" " ++ style ++ " />"
