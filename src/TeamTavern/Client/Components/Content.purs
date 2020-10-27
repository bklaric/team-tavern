module TeamTavern.Client.Components.Content where

import Halogen.HTML as HH
import TeamTavern.Client.Snippets.Class as HS

content = HH.div [ HS.class_ "content" ]

wideContent = HH.div [ HS.class_ "wide-content" ]

singleContent = HH.div [ HS.class_ "single-content" ]

contentHeader = HH.div [ HS.class_ "content-header" ]

contentHeading text = HH.h1 [ HS.class_ "content-heading" ] [ HH.text text ]

contentDescription text = HH.p [ HS.class_ "content-description" ] [ HH.text text ]
