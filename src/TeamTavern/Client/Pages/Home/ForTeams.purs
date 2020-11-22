module Client.Pages.Home.ForTeams where

import Halogen.HTML as HH
import TeamTavern.Client.Components.Landing (landingSection, landingSectionButton, landingSectionDescription, landingSectionHeading, landingSectionImage, landingSectionSubheading, landingSectionText)

forTeams onClick =
    landingSection
    [ landingSectionImage "/images/player-2.png"
    , landingSectionText
        [ landingSectionHeading "fas fa-users" "Looking for players?"
        , landingSectionDescription "Recruit new members for your team and expand your online community in five easy steps:"
        , landingSectionSubheading "1. Tell us about your team"
        , landingSectionDescription "Describe your team to find players that fit best into your team."
        , landingSectionSubheading "2. Choose a game"
        , landingSectionDescription "Choose one of the featured games to create your first team profile."
        , landingSectionSubheading "3. Fill out your team's game profile"
        , landingSectionDescription "Tell us about your team's ambitions and what you're looking for in new team members."
        , landingSectionSubheading "4. Create your account"
        , landingSectionDescription "Finish creating your account by entering your email address and password."
        , landingSectionSubheading "5. You're done!"
        , landingSectionDescription "Your account, your team and your first team profile are created. You're ready to find your new teammates!"
        , landingSectionButton "Create team profile" onClick
        ]
    ]
