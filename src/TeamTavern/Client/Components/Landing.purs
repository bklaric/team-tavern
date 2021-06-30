module TeamTavern.Client.Components.Landing where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Picture (picture)
import TeamTavern.Client.Snippets.Class as HS
import Web.UIEvent.MouseEvent (MouseEvent)

landingSection :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
landingSection content =
    HH.div [ HS.class_ "landing-section" ]
    [ HH.div [ HS.class_ "landing-section-content" ] content ]

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

landingSectionButton :: forall slots action.
    String -> String -> (MouseEvent -> action) -> HH.HTML slots action
landingSectionButton text url onClick =
    HH.a
    [ HS.class_ "landing-section-button"
    , HP.href url
    , HE.onClick $ Just <<< onClick
    ]
    [ HH.text text ]

landingSectionImage :: forall slots action. Maybe String -> String -> HH.HTML slots action
landingSectionImage title baseSrc =
    picture "landing-section-image" (maybe "Video game characters" (_ <> " character") title) baseSrc
