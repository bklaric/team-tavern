module TeamTavern.Client.Components.Boarding.Boarding where

import Halogen.HTML as HH
import TeamTavern.Client.Snippets.Class as HS

boarding :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
boarding = HH.div [ HS.class_ "boarding" ]

boardingStep :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
boardingStep = HH.div [ HS.class_ "boarding-step" ]

boardingHeading :: ∀ slots action. String -> HH.HTML slots action
boardingHeading text = HH.h1 [ HS.class_ "boarding-heading" ] [ HH.text text ]

boardingDescription :: ∀ slots action. String -> HH.HTML slots action
boardingDescription text = HH.p [ HS.class_ "boarding-description" ] [ HH.text text ]

boardingButtons :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
boardingButtons = HH.div [ HS.class_ "boarding-buttons" ]
