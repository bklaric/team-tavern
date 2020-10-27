module TeamTavern.Client.Components.Button where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import TeamTavern.Client.Snippets.Class as HS

regularIconButton :: forall action slots. String -> String -> action -> HH.HTML slots action
regularIconButton icon text onClick =
    HH.button
    [ HS.class_ "regular-button"
    , HE.onClick $ const $ Just onClick
    ]
    [ HH.i [ HS.class_ "fa fa-edit button-icon" ] []
    , HH.text text
    ]
