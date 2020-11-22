module Client.Pages.Home.ForTeams where

import Prelude

import Halogen.HTML as HH
import TeamTavern.Client.Components.Landing (landingSection, landingSectionButton, landingSectionDescription, landingSectionHeading, landingSectionImage, landingSectionSubheading, landingSectionText)

forTeams :: forall slots action. action -> HH.HTML slots action
forTeams onClick =
    landingSection
    [ landingSectionImage "/images/player-2.png"
    , landingSectionText $
        [ landingSectionHeading "fas fa-users" "Looking for players?"
        , landingSectionDescription "Recruit new members for your team and expand your online community in four easy steps:"
        , landingSectionSubheading "1. Tell us about your team"
        , landingSectionDescription "Describe your team to find players that fit best into your team."
        , landingSectionSubheading "2. Choose a game"
        , landingSectionDescription "Choose one of the featured games to create your first team profile."
        , landingSectionSubheading "3. Fill out your team's game profile"
        , landingSectionDescription "Tell us about your team's ambitions and what you're looking for in new team members."
        , landingSectionSubheading "4. Create your account"
        , landingSectionDescription "Finish creating your account by entering your email address and password."
        , landingSectionSubheading "You're done!"
        , landingSectionDescription "Your account, your team and your first team profile are created. You're ready to find your new teammates!"
        , landingSectionButton "Create team profile" onClick
        ]
    ]

forTeams' :: forall slots action. String -> action -> HH.HTML slots action
forTeams' title onClick =
    landingSection
    [ landingSectionImage "/images/player-2.png"
    , landingSectionText $
        [ landingSectionHeading "fas fa-users" "Looking for players?"
        , landingSectionDescription "Recruit new members for your team and expand your online community in three easy steps:"
        , landingSectionSubheading "1. Tell us about your team"
        , landingSectionDescription "Describe your team to find players that fit best into your team."
        , landingSectionSubheading $ "2. Fill out your team's " <> title <> " profile"
        , landingSectionDescription "Tell us about your team's ambitions and what you're looking for in new team members."
        , landingSectionSubheading "3. Create your account"
        , landingSectionDescription "Finish creating your account by entering your email address and password."
        , landingSectionSubheading "You're done!"
        , landingSectionDescription $ "Your account, your team and your " <> title <> " team profile are created. You're ready to find your new teammates!"
        , landingSectionButton "Create team profile" onClick
        ]
    ]
