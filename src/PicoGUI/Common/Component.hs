{-#LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module PicoGUI.Common.Component where

import Data.Text
import qualified PicoGUI.Common.Style as Style

-- standard components
data StandardComponents = 
    TextLabel { -- one line of text with transparent background
      cid              :: Text
    , labelText        :: Text 
    , defaultFontStyle :: Style.Font  
    , onHoverFontStyle :: Style.Font
    }  
  | Panel {
      cid               :: Text
    , defaultPanelStyle :: Style.Panel
    , onHoverPanelStyle :: Style.Panel
  }
  | Button { -- not just a button, but any text in a panel - so can be decorated in different ways
      cid                 :: Text  
    , labelText           :: Text
    , defaultButtonStyle  :: Style.VisualClass
    , onHoverButtonStyle  :: Style.VisualClass 
    }
  deriving (Show, Eq)
