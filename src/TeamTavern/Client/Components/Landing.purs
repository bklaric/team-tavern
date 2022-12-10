module TeamTavern.Client.Components.Landing where

import Prelude

import Data.Maybe (Maybe, maybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Anchor (iconAnchor)
import TeamTavern.Client.Components.Picture (picture)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Client.Snippets.SocialMediaUrls (discordUrl, redditUrl, steamId, twitterUrl)
import Web.UIEvent.MouseEvent (MouseEvent)

landingSection :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
landingSection content =
    HH.div [ HS.class_ "landing-section" ]
    [ HH.div [ HS.class_ "landing-section-content" ] content ]

landingSectionText :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
landingSectionText = HH.div [ HS.class_ "landing-section-text" ]

landingSectionHeading :: ∀ slots action. String -> String -> HH.HTML slots action
landingSectionHeading icon heading =
    HH.h2
    [ HS.class_ "landing-section-heading" ]
    [ HH.i [ HS.class_ $ icon <> " landing-section-heading-icon" ] []
    , HH.text heading
    ]

landingSectionSubheading :: ∀ slots action. String -> HH.HTML slots action
landingSectionSubheading subheading =
    HH.h3 [ HS.class_ "landing-section-subheading" ] [ HH.text subheading ]

landingSectionDescription :: ∀ slots action. String -> HH.HTML slots action
landingSectionDescription description =
    HH.p [ HS.class_ "landing-section-description" ] [ HH.text description ]

landingSectionButtons :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
landingSectionButtons = HH.div [ HS.class_ "landing-section-buttons" ]

landingSectionButton :: ∀ slots action.
    String -> String -> (MouseEvent -> action) -> HH.HTML slots action
landingSectionButton text url onClick =
    HH.a
    [ HS.class_ "landing-section-button"
    , HP.href url
    , HE.onClick onClick
    ]
    [ HH.text text ]

landingSectionImage :: ∀ slots action. Maybe String -> String -> HH.HTML slots action
landingSectionImage title baseSrc =
    picture "landing-section-image" (maybe "Video game characters" (_ <> " character") title) baseSrc

landingSectionConnect :: ∀ slots action. HH.HTML slots action
landingSectionConnect =
    HH.div [ HS.class_ "landing-section-connect" ]
    [ iconAnchor discordUrl "TeamTavern Discord server" "fab fa-discord"
    , iconAnchor redditUrl "TeamTavern subreddit" "fab fa-reddit"
    , iconAnchor steamId "TeamTavern Steam group" "fab fa-steam"
    , iconAnchor twitterUrl "TeamTavern Twitter account" "fab fa-twitter"
    ]
