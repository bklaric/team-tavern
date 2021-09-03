module TeamTavern.Client.Components.Content where

import Prelude

import Halogen.HTML as HH
import TeamTavern.Client.Snippets.Class as HS

content :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
content = HH.div [ HS.class_ "content" ]

wideContent :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
wideContent = HH.div [ HS.class_ "wide-content" ]

singleContent :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
singleContent = HH.div [ HS.class_ "single-content" ]

contentHeader :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
contentHeader = HH.div [ HS.class_ "content-header" ]

contentHeaderSection :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
contentHeaderSection = HH.div [ HS.class_ "content-header-section" ]

contentHeading :: forall slots action. String -> HH.HTML slots action
contentHeading text = HH.h1 [ HS.class_ "content-heading" ] [ HH.text text ]

contentHeading' :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
contentHeading' content' = HH.h1 [ HS.class_ "content-heading" ] content'

contentHeadingFaIcon :: forall slots action. String -> HH.HTML slots action
contentHeadingFaIcon icon = HH.i [ HS.class_ $ icon <> " content-heading-fa-icon" ] []

contentDescription :: forall slots action. String -> HH.HTML slots action
contentDescription text = HH.p [ HS.class_ "content-description" ] [ HH.text text ]

contentColumns :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
contentColumns content' = HH.div [ HS.class_ "content-columns" ] content'
