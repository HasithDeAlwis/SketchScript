----------------------------------------------------
---------------- BASIC UI STRUCTURE ----------------
----------------------------------------------------

module SketchScriptTypes where

data Element = ElementContainer Container | ElementWidget Widget
  deriving (Show)

data Widget = Widget WidgetType Sizing Content
  deriving (Show)

data Container = Container ContainerType Sizing Style [Element]
  deriving (Show)

-------------------------------------------------------
------------------ CORE UI ELEMENTS -------------------
-------------------------------------------------------

data WidgetType
  = Button
  | InputBox
  | TextField
  | Image
  deriving (Show)

-- TODO
-- \| Checkbox | RadioButton | Dropdown
-- \| Slider | ProgressBar | ListBox
-- \| TextArea | ToggleSwitch

data ContainerType = Box -- keep simple for now, will expand later
  deriving (Show)

-- TOOD
-- \| Grid -- for layout purposes
-- \| Flex -- for flexible layouts
-- \| Stack -- for stacking elements vertically or horizontally

newtype Content = Content (Maybe String)
  deriving (Show)

-------------------------------------------------------
---------------- SIZING AND STYLING -------------------
-------------------------------------------------------

newtype Sizing = Sizing (Maybe Float) -- A bit overengineered, but allows for more flexibility
  deriving (Show)

-- TODO
--   | Fixed Int -- Fixed size in pixels
--   | Percentage Float -- Percentage of the parent container's size

data Style
  = Flex Justify
  | Grid ColsAndRows
  deriving (Show)

data Justify
  = Start
  | Center
  | End
  | SpaceBetween
  | SpaceAround
  deriving (Show)

data ColsAndRows = ColsAndRows
  { cols :: Int,
    rows :: Int
  }
  deriving (Show)
