module TeamTavern.Client.Components.Button where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import TeamTavern.Client.Snippets.Class as HS

button :: ∀ action slots. String -> String -> String -> action -> HH.HTML slots action
button class_ icon text onClick =
    HH.button
    [ HS.class_ class_
    , HE.onClick $ const onClick
    ]
    [ HH.i [ HS.class_ $ icon <> " button-icon" ] []
    , HH.text text
    ]

regularButton :: ∀ action slots. String -> String -> action -> HH.HTML slots action
regularButton = button "regular-button"

regularButton_ :: ∀ action slots. String -> action -> HH.HTML slots action
regularButton_ = button "regular-button" ""

primaryButton :: ∀ action slots. String -> String -> action -> HH.HTML slots action
primaryButton = button "primary-button"

primaryButton_ :: ∀ action slots. String -> action -> HH.HTML slots action
primaryButton_ = button "primary-button" ""

secondaryButton :: ∀ action slots. String -> String -> action -> HH.HTML slots action
secondaryButton = button "secondary-button"

secondaryButton_ :: ∀ action slots. String -> action -> HH.HTML slots action
secondaryButton_ = button "secondary-button" ""
