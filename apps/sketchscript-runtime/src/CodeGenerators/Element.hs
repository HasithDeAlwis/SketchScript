module CodeGenerators.Element where

import CodeGenerators.Utils
import CodeGenerators.Widget
import SketchScriptTypes

genElement :: Element -> String
genElement (ElementContainer x) = genContainer x
genElement (ElementWidget x) = genWidget x

genContainer :: Container -> String
genContainer (Container Box sizing styles children) =
  "<div class=\""
    ++ genTailwindContainerClasses sizing styles children
    ++ "\">\n"
    ++ concatMap genElement children
    ++ "\n</div>"
