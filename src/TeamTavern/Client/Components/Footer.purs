module TeamTavern.Client.Components.Footer where

import Prelude

import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Anchor (iconAnchor)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.SocialMediaUrls (discordUrl, redditUrl, steamId, twitterUrl)
import Type.Proxy (Proxy(..))

type ChildSlots children =
    ( aboutAnchor :: Slot___
    , privacyAnchor :: Slot___
    , discordAnchor :: Slot___
    , redditAnchor :: Slot___
    , steamAnchor :: Slot___
    , twitterAnchor :: Slot___
    | children )

footer :: ∀ monad action children. MonadEffect monad =>
    H.ComponentHTML action (ChildSlots children) monad
footer = HH.div [ HP.class_ $ HH.ClassName "footer" ]
    [ HH.div [ HP.class_ $ HH.ClassName "footer-content" ]
        [ HH.div_
            [ navigationAnchor (Proxy :: _ "aboutAnchor")
                { path: "/about", content: HH.text "About" }
            , navigationAnchor (Proxy :: _ "privacyAnchor")
                { path: "/privacy", content: HH.text "Privacy Policy" }
            ]
        , HH.div_
            [ iconAnchor discordUrl "TeamTavern Discord server" "fab fa-discord footer-icon"
            , iconAnchor redditUrl "TeamTavern subreddit" "fab fa-reddit footer-icon"
            , iconAnchor steamId "TeamTavern Steam group" "fab fa-steam footer-icon"
            , iconAnchor twitterUrl "TeamTavern Twitter account" "fab fa-twitter footer-icon"
            ]
        ]
    ]
