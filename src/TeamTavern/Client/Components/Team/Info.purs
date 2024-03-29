module TeamTavern.Client.Components.Team.Info where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HP
import TeamTavern.Client.Snippets.Class as HS
import Web.UIEvent.MouseEvent (MouseEvent)

infoContainer :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
infoContainer = HH.div [ HS.class_ "info-container" ]

infoIcon :: ∀ slots action. (MouseEvent -> action) -> HH.HTML slots action
infoIcon onClick =
    HH.i
    [ HS.class_ "fas fa-question-circle info-icon"
    , HP.onClick onClick
    ]
    []

info :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
info = HH.div [ HS.class_ "info" ]

infoText :: ∀ slots action. String -> HH.HTML slots action
infoText text = HH.p [ HS.class_ "info-text" ] [ HH.text text ]

infoList :: ∀ slots action. Array String -> HH.HTML slots action
infoList items = HH.ul [ HS.class_ "info-list" ] (items <#> \text -> HH.li_ [ HH.text text ])
