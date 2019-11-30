module TeamTavern.Client.Components.Footer where

import Prelude

import Data.Symbol (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Snippets.SocialMediaUrls (discordUrl, redditUrl, steamUrl, twitterUrl)

type ChildSlots children =
    ( aboutAnchor :: NavigationAnchor.Slot Unit
    , discordAnchor :: NavigationAnchor.Slot Unit
    , redditAnchor :: NavigationAnchor.Slot Unit
    , steamAnchor :: NavigationAnchor.Slot Unit
    , twitterAnchor :: NavigationAnchor.Slot Unit
    | children )

footer :: forall monad action children. MonadEffect monad =>
    H.ComponentHTML action (ChildSlots children) monad
footer = HH.div [ HP.class_ $ HH.ClassName "footer" ]
    [ HH.div [ HP.class_ $ HH.ClassName "footer-content" ]
        [ navigationAnchor (SProxy :: SProxy "aboutAnchor")
            { path: "/about", content: HH.text "About" }
        , HH.div_
            [ navigationAnchor (SProxy :: SProxy "discordAnchor")
                { path: discordUrl
                , content: HH.i
                    [ HP.class_ $ H.ClassName "fab fa-discord footer-icon" ] []
                }
            , navigationAnchor (SProxy :: SProxy "redditAnchor")
                { path: redditUrl
                , content: HH.i
                    [ HP.class_ $ H.ClassName "fab fa-reddit footer-icon" ] []
                }
            , navigationAnchor (SProxy :: SProxy "steamAnchor")
                { path: steamUrl
                , content: HH.i
                    [ HP.class_ $ H.ClassName "fab fa-steam footer-icon" ] []
                }
            , navigationAnchor (SProxy :: SProxy "twitterAnchor")
                { path: twitterUrl
                , content: HH.i
                    [ HP.class_ $ H.ClassName "fab fa-twitter footer-icon" ] []
                }
            ]
        ]
    ]
