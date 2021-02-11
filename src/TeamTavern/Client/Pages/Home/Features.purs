module TeamTavern.Client.Pages.Home.Features where

import Prelude

import CSS as CSS
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import TeamTavern.Client.Components.Landing (landingSectionButton)
import TeamTavern.Client.Snippets.Class as HS
import Web.UIEvent.MouseEvent (MouseEvent)

features :: forall slots action. (MouseEvent -> action) -> HH.HTML slots action
features createAccount =
    HH.div
    [ HS.class_ "features"
    , HC.style $ CSS.backgroundImage $ CSS.fromString $
        "linear-gradient(hsla(20, 20%, 19%, 0.8),hsla(20, 20%, 19%, 0.8)), url(/images/features.jpg)"
    ]
    [ HH.div [ HS.class_ "features-content" ]
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
                    [ HH.i [ HS.class_ "fas fa-bolt feature-icon" ] []
                    , HH.text "Fast and simple setup"
                    ]
                , HH.p  [ HS.class_ "features-feature-description" ]
                    [ HH.text """Create your first player or team profile in seconds.
                    No email address required. Search through all existing players and teams without
                    creating an account.""" ]
                ]
            ]
        , landingSectionButton "Start finding teammates" "/preboarding/start" createAccount
        ]
    ]

features' :: forall slots action. String -> String -> (MouseEvent -> action) -> HH.HTML slots action
features' handle title createAccount =
    HH.div
    [ HS.class_ "features"
    , HC.style $ CSS.backgroundImage $ CSS.fromString $
        "linear-gradient(hsla(20, 20%, 19%, 0.8),hsla(20, 20%, 19%, 0.8)), url(/images/" <> handle <> "/features.jpg)"
    ]
    [ HH.div [ HS.class_ "features-content" ]
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
                    [ HH.i [ HS.class_ "fas fa-bolt feature-icon" ] []
                    , HH.text "Fast and simple setup"
                    ]
                , HH.p  [ HS.class_ "features-feature-description" ]
                    [ HH.text """Create your first player or team profile in seconds.
                    No email address required. Search through all existing players and teams without
                    creating an account.""" ]
                ]
            ]
        , landingSectionButton "Start finding teammates" "/preboarding/start" createAccount
        ]
    ]
