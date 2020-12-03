module TeamTavern.Client.Components.Card where

import Halogen.HTML as HH
import TeamTavern.Client.Snippets.Class as HS

card :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
card = HH.div [ HS.class_ "card" ]

cardHeader :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
cardHeader = HH.div [ HS.class_ "card-header" ]

cardHeading' :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
cardHeading' content = HH.h2 [ HS.class_ "card-heading"] content

cardHeading :: forall slots action. String -> HH.HTML slots action
cardHeading heading = cardHeading' [ HH.text heading ]

cardSubheading :: forall slots action. String -> HH.HTML slots action
cardSubheading subheading = HH.span [ HS.class_ "card-subheading"] [ HH.text subheading ]

cardSection :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
cardSection = HH.div [ HS.class_ "card-section" ]

cardSectionHeader :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
cardSectionHeader = HH.div [ HS.class_ "card-section-header" ]

cardSectionHeading :: forall slots action. String -> HH.HTML slots action
cardSectionHeading heading = HH.h3 [ HS.class_ "card-section-heading" ] [ HH.text heading ]
