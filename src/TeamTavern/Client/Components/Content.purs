module TeamTavern.Client.Components.Content where

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

contentHeading :: forall slots action. String -> HH.HTML slots action
contentHeading text = HH.h1 [ HS.class_ "content-heading" ] [ HH.text text ]

contentDescription :: forall slots action. String -> HH.HTML slots action
contentDescription text = HH.p [ HS.class_ "content-description" ] [ HH.text text ]
