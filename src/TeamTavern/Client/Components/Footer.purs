module TeamTavern.Client.Components.Footer where

import Prelude

import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Client.Snippets.SocialMediaUrls (discordUrl, redditUrl, steamUrl, twitterUrl)

type ChildSlots children =
    ( aboutAnchor :: NavigationAnchor.Slot Unit
    , privacyAnchor :: NavigationAnchor.Slot Unit
    , discordAnchor :: NavigationAnchor.Slot Unit
    , redditAnchor :: NavigationAnchor.Slot Unit
    , steamAnchor :: NavigationAnchor.Slot Unit
    , twitterAnchor :: NavigationAnchor.Slot Unit
    | children )

footer :: forall monad action children. MonadEffect monad =>
    H.ComponentHTML action (ChildSlots children) monad
footer = HH.div [ HP.class_ $ HH.ClassName "footer" ]
    [ HH.div [ HP.class_ $ HH.ClassName "footer-content" ]
        [ HH.div_
            [ navigationAnchor (SProxy :: SProxy "aboutAnchor")
                { path: "/about", content: HH.text "About" }
            , HH.a
                [ HS.class_ "nn-cmp-show"
                , HP.prop (H.PropName "style") "margin-left: 21px; margin-right: 21px;"
                , HP.href "#"
                ]
                [ HH.text "Manage Cookie Settings" ]
            , navigationAnchor (SProxy :: SProxy "privacyAnchor")
                { path: "/privacy", content: HH.text "Privacy Policy" }
            ]
        , HH.div_
            [ HH.a [ HP.href discordUrl ]
                [ HH.i [ HP.class_ $ H.ClassName "fab fa-discord footer-icon" ] [] ]
            , HH.a [ HP.href redditUrl ]
                [ HH.i [ HP.class_ $ H.ClassName "fab fa-reddit footer-icon" ] [] ]
            , HH.a [ HP.href steamUrl ]
                [ HH.i [ HP.class_ $ H.ClassName "fab fa-steam footer-icon" ] [] ]
            , HH.a [ HP.href twitterUrl ]
                [ HH.i [ HP.class_ $ H.ClassName "fab fa-twitter footer-icon" ] [] ]
            ]
        ]
    ]
