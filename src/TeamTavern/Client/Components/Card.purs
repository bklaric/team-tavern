module TeamTavern.Client.Components.Card where

import Halogen.HTML as HH
import TeamTavern.Client.Snippets.Class as HS

card :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
card = HH.div [ HS.class_ "card" ]

cardHeader :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
cardHeader = HH.div [ HS.class_ "card-header" ]

cardHeading' :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
cardHeading' content = HH.h2 [ HS.class_ "card-heading"] content

cardHeading :: ∀ slots action. String -> HH.HTML slots action
cardHeading heading = cardHeading' [ HH.text heading ]

cardSubheading :: ∀ slots action. String -> HH.HTML slots action
cardSubheading subheading = HH.span [ HS.class_ "card-subheading"] [ HH.text subheading ]

cardSection :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
cardSection = HH.div [ HS.class_ "card-section" ]

cardSectionHeader :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
cardSectionHeader = HH.div [ HS.class_ "card-section-header" ]

cardSectionHeading :: ∀ slots action. String -> HH.HTML slots action
cardSectionHeading heading = HH.h3 [ HS.class_ "card-section-heading" ] [ HH.text heading ]
