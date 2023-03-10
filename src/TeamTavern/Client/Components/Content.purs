module TeamTavern.Client.Components.Content where

import Prelude

import Halogen.HTML as HH
import TeamTavern.Client.Snippets.Class as HS

content :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
content = HH.div [ HS.class_ "content" ]

wideContent :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
wideContent = HH.div [ HS.class_ "wide-content" ]

actualContent :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
actualContent = HH.div [HS.class_ "actual-content"]

singleContent :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
singleContent = HH.div [ HS.class_ "single-content" ]

contentHeader :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
contentHeader = HH.div [ HS.class_ "content-header" ]

contentHeaderSection :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
contentHeaderSection = HH.div [ HS.class_ "content-header-section" ]

contentHeading :: ∀ slots action. String -> HH.HTML slots action
contentHeading text = HH.h1 [ HS.class_ "content-heading" ] [ HH.text text ]

contentHeading' :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
contentHeading' content' = HH.h1 [ HS.class_ "content-heading" ] content'

contentHeadingFaIcon :: ∀ slots action. String -> HH.HTML slots action
contentHeadingFaIcon icon = HH.i [ HS.class_ $ icon <> " content-heading-fa-icon" ] []

contentDescription :: ∀ slots action. String -> HH.HTML slots action
contentDescription text = HH.p [ HS.class_ "content-description" ] [ HH.text text ]

contentColumns :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
contentColumns content' = HH.div [ HS.class_ "content-columns" ] content'
