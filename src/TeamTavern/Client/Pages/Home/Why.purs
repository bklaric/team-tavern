module TeamTavern.Client.Pages.Home.Why where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

why :: forall t1 t2. HH.HTML t2 t1
why =
    HH.div [ HP.class_ $ HH.ClassName "why" ]
    [ HH.h2 [ HP.class_ $ HH.ClassName "why-heading" ]
        [ HH.text "Why TeamTavern" ]
    , HH.h3 [ HP.class_ $ HH.ClassName "why-reason-title" ]
        [ HH.text "Unparalleled team finding platform" ]
    , HH.p  [ HP.class_ $ HH.ClassName "why-reason-description" ]
        [ HH.text """We aim to make TeamTavern the most versatile team finding
        platform and we're always looking for ways to improve.""" ]
    , HH.h3 [ HP.class_ $ HH.ClassName "why-reason-title" ]
        [ HH.text "Responsive support and development" ]
    , HH.p  [ HP.class_ $ HH.ClassName "why-reason-description" ]
        [ HH.text """Have a suggestion? We always listen to feedback to ensure
        we build the platform players want.""" ]
    ]
