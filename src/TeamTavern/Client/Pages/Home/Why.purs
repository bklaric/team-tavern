module TeamTavern.Client.Pages.Home.Why where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

why :: ∀ slots action. HH.HTML slots action
why =
    HH.div [ HP.class_ $ HH.ClassName "why" ]
    [ HH.h2 [ HP.class_ $ HH.ClassName "why-heading" ]
        [ HH.text "Why TeamTavern" ]
    , HH.h3 [ HP.class_ $ HH.ClassName "why-reason-title" ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-laptop why-icon" ] []
        , HH.text "Unparalleled team finding platform"
        ]
    , HH.p [ HP.class_ $ HH.ClassName "why-reason-description" ]
        [ HH.text """We aim to make TeamTavern the most versatile team finding
        platform and we're always looking for ways to improve.""" ]
    , HH.h3 [ HP.class_ $ HH.ClassName "why-reason-title" ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-comments why-icon" ] []
        , HH.text "Responsive support and development"
        ]
    , HH.p [ HP.class_ $ HH.ClassName "why-reason-description" ]
        [ HH.text """Have a suggestion? We always listen to feedback to ensure
        we build the platform players want.""" ]
    ]
