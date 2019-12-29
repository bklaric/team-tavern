module TeamTavern.Client.Home.CallToAction where

import Prelude

import Data.Symbol (SProxy(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorClassed)

callToAction =
    HH.div [ HP.class_ $ HH.ClassName "call-to-action" ]
    [ HH.div [ HP.class_ $ HH.ClassName "call-to-action-content" ]
        [ HH.div [HP.class_ $ HH.ClassName "call-to-action-text" ]
            [ HH.h1 [HP.class_ $ HH.ClassName "call-to-action-heading" ]
                [ HH.text "Find your esports teammates" ]
            , HH.p [ HP.class_ $ HH.ClassName "call-to-action-paragraph" ]
                [ HH.text $
                    "TeamTavern is an online platform for finding esports teammates. "
                    <> "Choose a game, browse player and team profiles and find your ideal teammates. "
                    <> "Create your own profile and let them find you."
                ]
            , navigationAnchorClassed (SProxy :: SProxy "callToActionButton")
                { class_: "call-to-action-button"
                , path: "register"
                , content: HH.text "Start finding teammates"
                }
            ]
        ]
    ]
