module TeamTavern.Client.Components.Button where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import TeamTavern.Client.Snippets.Class as HS

button :: forall action slots. String -> String -> String -> action -> HH.HTML slots action
button class_ icon text onClick =
    HH.button
    [ HS.class_ class_
    , HE.onClick $ const $ Just onClick
    ]
    [ HH.i [ HS.class_ $ icon <> " button-icon" ] []
    , HH.text text
    ]

regularButton :: forall action slots. String -> String -> action -> HH.HTML slots action
regularButton = button "regular-button"

primaryButton :: forall action slots. String -> String -> action -> HH.HTML slots action
primaryButton = button "primary-button"

secondaryButton :: forall action slots. String -> String -> action -> HH.HTML slots action
secondaryButton = button "secondary-button"
