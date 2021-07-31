module TeamTavern.Client.Pages.Profiles.GameHeader (ProfileTab(..), Tab(..), Input, gameHeader) where

import Prelude

import Halogen.HTML as HH
import TeamTavern.Client.Components.Ads (descriptionLeaderboards)
import TeamTavern.Client.Components.Content (contentDescription, contentHeader, contentHeaderSection, contentHeading')
import TeamTavern.Client.Snippets.Class as HS

data ProfileTab = Players | Teams

data Tab = Profiles ProfileTab | Competitions

type Input = { title :: String, shortTitle :: String, tab :: Tab }

gameHeader :: forall slots action. Input -> HH.HTML slots action
gameHeader { title, shortTitle, tab } = HH.div_ $
    [ contentHeader
        [ contentHeaderSection
            [ contentHeading'
                case tab of
                Profiles Players -> [ HH.i [ HS.class_ "fas fa-user content-heading-icon" ] [], HH.text $ title <> " players" ]
                Profiles Teams -> [ HH.i [ HS.class_ "fas fa-users content-heading-icon" ] [], HH.text $ title <> " teams" ]
                Competitions -> [ HH.i [ HS.class_ "fas fa-trophy content-heading-icon" ] [], HH.text $ title <> " competitions" ]
            ]
        ]
    , contentDescription
        case tab of
        Profiles Players -> "Find " <> shortTitle <> " players looking for a team. Create your own player profile and let everyone know you're looking to team up."
        Profiles Teams -> "Find  " <> shortTitle <> " teams looking for players. Create your own team profile and recruit new members for your team."
        Competitions -> "Apply for open " <> shortTitle <> " leagues and tournaments and compete for prizes and boasting rights."
    ]
    <>
    -- if shortTitle == "Dota 2"
    -- then Array.singleton $
    --     HH.a [ HS.class_ "partner-banner", HP.href "https://discord.gg/d2zjFWxC8M", HP.target "_blank", HP.rel "noopener", HP.title "Dota 2 Dream League Community" ]
    --     [ HH.span [ HS.class_ "partner-banner-detail" ] [ HH.text "Open league • EU servers • Sign-ups end July 17th" ]
    --     , HH.img [ HS.class_ "partner-banner-image", HP.src $ "https://cdn.discordapp.com/attachments/861682299107278868/862348698603225108/dota_banner_1.jpg" ]
    --     ]
    -- else [ descriptionLeaderboard ]
    descriptionLeaderboards
