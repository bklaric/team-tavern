module TeamTavern.Client.Pages.Home.Features where

import Prelude

import Halogen.HTML as HH
import TeamTavern.Client.Components.Landing (landingSectionButton)
import TeamTavern.Client.Snippets.Class as HS

features :: forall slots action. action -> HH.HTML slots action
features createAccount =
    HH.div [ HS.class_ "features" ]
    [ HH.h2 [ HS.class_ "features-heading" ]
        [ HH.text "All you need to find great teammates" ]
    , HH.div [ HS.class_ "features-grid" ]
        [ HH.div_
            [ HH.h3 [ HS.class_ "features-feature-heading" ]
                [ HH.i [ HS.class_ "fas fa-user feature-icon" ] []
                , HH.text "For players"
                ]
            , HH.p  [ HS.class_ "features-feature-description" ]
                [ HH.text """Are you a single player looking for a team?
                Create your player profile and let everyone know you're looking
                to team up. Find and join existing teams looking to recruit new
                members.""" ]
            ]
        , HH.div_
            [ HH.h3 [ HS.class_ "features-feature-heading" ]
                [ HH.i [ HS.class_ "fas fa-users feature-icon" ] []
                , HH.text "For teams"
                ]
            , HH.p  [ HS.class_ "features-feature-description" ]
                [ HH.text """Do you want to build a team or expand your existing
                online community? Create your team profile and let prospecting players
                know you're looking for new members. Find and invite like-minded players
                of matching skills and ambitions.""" ]
            ]
        , HH.div_
            [ HH.h3 [ HS.class_ "features-feature-heading" ]
                [ HH.i [ HS.class_ "fas fa-address-card feature-icon" ] []
                , HH.text "Detailed profiles"
                ]
            , HH.p  [ HS.class_ "features-feature-description" ]
                [ HH.text """Describe yourself or your team in great detail. Fill out
                personal details such as age, location, languages and mic usage as well
                as game specific details. Filter existing players and teams on all
                details to get the most relevant results.""" ]
            ]
        , HH.div_
            [ HH.h3 [ HS.class_ "features-feature-heading" ]
                [ HH.i [ HS.class_ "fas fa-envelope feature-icon" ] []
                , HH.text "Built-in messaging"
                ]
            , HH.p  [ HS.class_ "features-feature-description" ]
                [ HH.text """Found an interesting player or team you want to play with?
                Message them directly on TeamTavern and team up in-game. Receive an
                email notification whenever someone messages you.""" ]
            ]
        ]
    , landingSectionButton "Start finding teammates" createAccount
    ]

features' :: forall slots action. String -> action -> HH.HTML slots action
features' title createAccount =
    HH.div [ HS.class_ "features" ]
    [ HH.h2 [ HS.class_ "features-heading" ]
        [ HH.text "All you need to find great teammates" ]
    , HH.div [ HS.class_ "features-grid" ]
        [ HH.div_
            [ HH.h3 [ HS.class_ "features-feature-heading" ]
                [ HH.i [ HS.class_ "fas fa-user feature-icon" ] []
                , HH.text "For players"
                ]
            , HH.p  [ HS.class_ "features-feature-description" ]
                [ HH.text """Are you a single player looking for a team?
                Create your player profile and let everyone know you're looking
                to team up. Find and join existing teams looking to recruit new
                members.""" ]
            ]
        , HH.div_
            [ HH.h3 [ HS.class_ "features-feature-heading" ]
                [ HH.i [ HS.class_ "fas fa-users feature-icon" ] []
                , HH.text "For teams"
                ]
            , HH.p  [ HS.class_ "features-feature-description" ]
                [ HH.text """Do you want to build a team or expand your existing
                online community? Create your team profile and let prospecting players
                know you're looking for new members. Find and invite like-minded players
                of matching skills and ambitions.""" ]
            ]
        , HH.div_
            [ HH.h3 [ HS.class_ "features-feature-heading" ]
                [ HH.i [ HS.class_ "fas fa-address-card feature-icon" ] []
                , HH.text "Detailed profiles"
                ]
            , HH.p  [ HS.class_ "features-feature-description" ]
                [ HH.text $ """Describe yourself or your team in great detail. Fill out
                personal details such as age, location, languages and mic usage as well
                as """ <> title <> """ specific details. Filter existing players and teams on all
                details to get the most relevant results.""" ]
            ]
        , HH.div_
            [ HH.h3 [ HS.class_ "features-feature-heading" ]
                [ HH.i [ HS.class_ "fas fa-envelope feature-icon" ] []
                , HH.text "Built-in messaging"
                ]
            , HH.p  [ HS.class_ "features-feature-description" ]
                [ HH.text """Found an interesting player or team you want to play with?
                Message them directly on TeamTavern and team up in-game. Receive an
                email notification whenever someone messages you.""" ]
            ]
        ]
    , landingSectionButton "Start finding teammates" createAccount
    ]
