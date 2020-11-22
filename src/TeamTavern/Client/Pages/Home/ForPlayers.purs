module TeamTavern.Client.Pages.Home.ForPlayers where

import Prelude

import Halogen.HTML as HH
import TeamTavern.Client.Components.Landing (landingSection, landingSectionButton, landingSectionDescription, landingSectionHeading, landingSectionImage, landingSectionSubheading, landingSectionText)

forPlayers :: forall slots action. action -> HH.HTML slots action
forPlayers onClick =
    landingSection
    [ landingSectionText
        [ landingSectionHeading "fas fa-user" "Looking for a team?"
        , landingSectionDescription "Let everyone know you're looking to team up in four easy steps:"
        , landingSectionSubheading "1. Tell us about yourself"
        , landingSectionDescription "Describe yourself to find like-minded gamers."
        , landingSectionSubheading "2. Choose a game"
        , landingSectionDescription "Choose one of the featured games to create your first player profile."
        , landingSectionSubheading "3. Fill out your game profile"
        , landingSectionDescription "Share your in-game stats, achievements and ambitions to find equally skilled teammates."
        , landingSectionSubheading "4. Create your account"
        , landingSectionDescription "Finish creating your account by entering your email address and password."
        , landingSectionSubheading "You're done!"
        , landingSectionDescription "Your account and your first player profile are created. You're ready to find your new teammates!"
        , landingSectionButton "Create player profile" onClick
        ]
    , landingSectionImage "/images/player-1.png"
    ]


forPlayers' :: forall slots action. String -> action -> HH.HTML slots action
forPlayers' title onClick =
    landingSection
    [ landingSectionText $
        [ landingSectionHeading "fas fa-user" "Looking for a team?"
        , landingSectionDescription "Let everyone know you're looking to team up in three easy steps:"
        , landingSectionSubheading "1. Tell us about yourself"
        , landingSectionDescription "Describe yourself to find like-minded gamers."
        , landingSectionSubheading $ "2. Fill out your " <> title <> " profile"
        , landingSectionDescription "Share your in-game stats, achievements and ambitions to find equally skilled teammates."
        , landingSectionSubheading "3. Create your account"
        , landingSectionDescription "Finish creating your account by entering your email address and password."
        , landingSectionSubheading "You're done!"
        , landingSectionDescription $ "Your account and your " <> title <> " player profile are created. You're ready to find your new teammates!"
        , landingSectionButton "Create player profile" onClick
        ]
    , landingSectionImage "/images/player-1.png"
    ]
