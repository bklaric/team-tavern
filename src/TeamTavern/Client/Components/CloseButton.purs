module TeamTavern.Client.Components.CloseButton where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

closeButton :: forall action slots. action -> HH.HTML slots action
closeButton closeAction = HH.button
    [ HP.class_ $ HH.ClassName "close-button"
    , HP.type_ HP.ButtonButton
    , HE.onClick $ const $ Just closeAction
    ]
    [ HH.i [ HP.class_ $ HH.ClassName "fas fa-times close-button-icon" ] [] ]
