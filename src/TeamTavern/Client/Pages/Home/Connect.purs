module TeamTavern.Client.Pages.Home.Connect where

import Prelude

import Halogen.HTML as HH
import TeamTavern.Client.Components.Landing (landingSection, landingSectionConnect, landingSectionDescription, landingSectionHeading, landingSectionText)

connect :: ∀ slots action. HH.HTML slots action
connect =
    landingSection
    [ landingSectionConnect
    , landingSectionText
        [ landingSectionHeading "fas fa-share-alt" "Connect with us!"
        , landingSectionDescription "Share your thoughts on TeamTavern, tell us your suggestions and find the latest updates and announcements on Discord, Twitter, Reddit and Steam. Additionally, join our Discord server to discuss featured games and find impromptu groups to play with."
        ]
    ]

connect' :: ∀ slots action. String -> HH.HTML slots action
connect' title =
    landingSection
    [ landingSectionConnect
    , landingSectionText
        [ landingSectionHeading "fas fa-share-alt" "Connect with us!"
        , landingSectionDescription $ "Share your thoughts on TeamTavern, tell us your suggestions and find the latest updates and announcements on Discord, Twitter, Reddit and Steam. Additionally, join our Discord server to discuss " <> title <> " and find impromptu groups to play with."
        ]
    ]
