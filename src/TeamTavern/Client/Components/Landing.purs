module TeamTavern.Client.Components.Landing where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS

landingSection :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
landingSection = HH.div [ HS.class_ "landing-section" ]

landingSectionText :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
landingSectionText = HH.div [ HS.class_ "landing-section-text" ]

landingSectionHeading :: forall slots action. String -> String -> HH.HTML slots action
landingSectionHeading icon heading =
    HH.h2
    [ HS.class_ "landing-section-heading" ]
    [ HH.i [ HS.class_ $ icon <> " landing-section-heading-icon" ] []
    , HH.text heading
    ]

landingSectionSubheading :: forall slots action. String -> HH.HTML slots action
landingSectionSubheading subheading =
    HH.h3 [ HS.class_ "landing-section-subheading" ] [ HH.text subheading ]

landingSectionDescription :: forall slots action. String -> HH.HTML slots action
landingSectionDescription description =
    HH.p [ HS.class_ "landing-section-description" ] [ HH.text description ]

landingSectionButtons :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
landingSectionButtons = HH.div [ HS.class_ "landing-section-buttons" ]

landingSectionButton :: forall slots action. String -> action -> HH.HTML slots action
landingSectionButton text onClick =
    HH.button
    [ HS.class_ "landing-section-button"
    , HE.onClick $ const $ Just onClick
    ]
    [ HH.text text ]

landingSectionImage :: forall slots action. String -> HH.HTML slots action
landingSectionImage src = HH.img [ HS.class_ "landing-section-image", HP.src src ]
