module TeamTavern.Client.Components.Landing where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS

landingSection = HH.div [ HS.class_ "landing-section" ]

landingSectionText = HH.div [ HS.class_ "landing-section-text" ]

landingSectionHeading icon heading =
    HH.h2
    [ HS.class_ "landing-section-heading" ]
    [ HH.i [ HS.class_ $ icon <> " landing-section-heading-icon" ] []
    , HH.text heading
    ]

landingSectionSubheading subheading =
    HH.h3 [ HS.class_ "landing-section-subheading" ] [ HH.text subheading ]

landingSectionDescription description =
    HH.p [ HS.class_ "landing-section-description" ] [ HH.text description ]

landingSectionButton text onClick =
    HH.button
    [ HS.class_ "landing-section-button"
    , HE.onClick $ const $ Just onClick
    ]
    [ HH.text text ]

landingSectionImage src = HH.img [ HS.class_ "landing-section-image", HP.src src ]
