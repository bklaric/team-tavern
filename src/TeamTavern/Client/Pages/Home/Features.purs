module TeamTavern.Client.Pages.Home.Features where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

features :: forall t1 t2. HH.HTML t2 t1
features =
    HH.div [ HP.class_ $ HH.ClassName "features" ]
    [ HH.h2 [ HP.class_ $ HH.ClassName "features-heading" ]
        [ HH.text "Amazing features" ]
    , HH.h3 [ HP.class_ $ HH.ClassName "features-feature-title" ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user feature-icon" ] []
        , HH.text "For players"
        ]
    , HH.p  [ HP.class_ $ HH.ClassName "features-feature-description" ]
        [ HH.text """Are you a single player looking for a team?
        Create your player profile and let everyone know you're looking
        to team up. Find and join existing teams looking to recruit new
        members.""" ]
    , HH.h3 [ HP.class_ $ HH.ClassName "features-feature-title" ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-users feature-icon" ] []
        , HH.text "For teams"
        ]
    , HH.p  [ HP.class_ $ HH.ClassName "features-feature-description" ]
        [ HH.text """Do you want to build a team or expand your existing
        online community? Create your team profile and let prospecting players
        know you're looking for new members. Find and invite like-minded players
        of matching skills and ambitions.""" ]
    , HH.h3 [ HP.class_ $ HH.ClassName "features-feature-title" ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-address-card feature-icon" ] []
        , HH.text "Detailed profiles"
        ]
    , HH.p  [ HP.class_ $ HH.ClassName "features-feature-description" ]
        [ HH.text """Describe yourself or your team in great detail. Fill out
        personal details such as age, location, languages and mic usage as well
        as game specific details. Filter existing players and teams on all
        details to get the most relevant results.""" ]
    , HH.h3 [ HP.class_ $ HH.ClassName "features-feature-title" ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-envelope feature-icon" ] []
        , HH.text "Built-in messaging"
        ]
    , HH.p  [ HP.class_ $ HH.ClassName "features-feature-description" ]
        [ HH.text """Found an interesting player or team you want to play with?
        Message them directly on TeamTavern and team up in-game. Receive an
        email notification whenever someone else messages you.""" ]
    ]
