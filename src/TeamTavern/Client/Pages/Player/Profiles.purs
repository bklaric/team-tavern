module TeamTavern.Client.Pages.Player.Profiles (profiles) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Array.Extra (full)
import Data.Monoid (guard)
import Type.Proxy (Proxy(..))
import Halogen.HTML as HH
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading, cardSection)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading4, detailColumns, textDetail)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Missing (missing)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Components.Player.ProfileDetails (PlatformIdSlots, profileDetails)
import TeamTavern.Client.Components.Profile (profileHeader, profileHeading', profileSubheading)
import TeamTavern.Client.Pages.Player.CreateProfileButton (createProfileButton)
import TeamTavern.Client.Pages.Player.CreateProfileButton as CreateProfileButton
import TeamTavern.Client.Pages.Player.PlayerProfileOptions (playerProfileOptions)
import TeamTavern.Client.Pages.Player.Status (Status(..))
import TeamTavern.Client.Pages.Profiles.TeamBadge (platformBadge)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Shared.Slot (QuerylessSlot)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Player.ViewPlayer as ViewPlayer

type ChildSlots children = PlatformIdSlots
    ( games :: NavigationAnchor.Slot String
    , createProfile :: CreateProfileButton.Slot
    , playerProfileOptions :: QuerylessSlot Unit String
    | children)

profiles
    :: ∀ action slots left
    .  ViewPlayer.OkContent
    -> Status
    -> (ViewPlayer.OkContentProfile -> action)
    -> HH.ComponentHTML action (ChildSlots slots) (Async left)
profiles player @ { profiles: profiles' } status showEditProfileModal =
    card $
    [ cardHeader $
        [ cardHeading "Profiles" ]
        <>
        guard (status == SignedInSelf)
        [ HH.slot (Proxy :: _ "createProfile") unit createProfileButton player absurd ]
    ]
    <>
    if Array.null profiles'
    then [ cardSection [ missing
        case status of
        SignedInSelf -> "You haven't create any profiles."
        _ -> "This player hasn't created any profiles." ] ]
    else profiles' <#> \profile -> let
        profileDetails' = profileDetails profile.fields profile.fieldValues profile.newOrReturning
        about = textDetail profile.about
        ambitions = textDetail profile.ambitions
        in
        cardSection $
        [ profileHeader $
            (if full profile.platforms.tail
            then
            [ HH.div [ HS.class_ "team-profile-heading-container" ] $
                [ profileHeading' (Proxy :: _ "games") profile.handle
                    ("/games/" <> profile.handle <> "/players") profile.title
                ]
                <> [ platformBadge profile.platform, profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds ]
            ]
            else
            [ HH.div_ $
                [ profileHeading' (Proxy :: _ "games") profile.handle
                    ("/games/" <> profile.handle <> "/players") profile.title
                ]
                <> [ divider, profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds ]
            ])
            <>
            [ playerProfileOptions { status, nickname: player.nickname, handle: profile.handle } $ showEditProfileModal profile ]
        ]
        <>
        guard (full profileDetails' || full about || full ambitions)
        [ detailColumns $
            guard (full profileDetails')
            [ detailColumn $ [ detailColumnHeading4 "Details" ] <> profileDetails' ]
            <>
            guard (full about || full ambitions)
            [ detailColumn $
                guard (full about) ([ detailColumnHeading4 "About" ] <> about)
                <>
                guard (full ambitions) ([ detailColumnHeading4 "Ambitions" ] <> ambitions)
            ]
        ]
