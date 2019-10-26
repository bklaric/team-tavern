module TeamTavern.Client.Home.CallToAction where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.RegisterForm (registerForm)

callToAction =
    HH.div [ HP.class_ $ HH.ClassName "call-to-action" ]
    [ HH.div [ HP.class_ $ HH.ClassName "call-to-action-content" ]
        [ HH.div [HP.class_ $ HH.ClassName "call-to-action-text" ]
            [ HH.h1 [HP.class_ $ HH.ClassName "call-to-action-heading" ]
                [ HH.text "Easily find your ideal esports teammates" ]
            , HH.p [HP.class_ $ HH.ClassName "call-to-action-paragraph"]
                [ HH.text $
                    "Tired of random matchmaking that results in ruined matches and wasted time? "
                    <> "TeamTavern connects you to other esports players. "
                    <> "Browse player profiles and find your ideal teammates for the games you play. "
                    <> "Create your own player profile and let them find you."
                ]
            ]
        , registerForm
        ]
    ]
