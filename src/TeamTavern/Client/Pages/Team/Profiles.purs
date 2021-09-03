module TeamTavern.Client.Pages.Team.Profiles (profiles) where

import Prelude

import Async (Async)
import Data.Array (sort)
import Data.Array as Array
import Data.Array.Extra (full)
import Data.Const (Const)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Button (regularButton)
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading, cardSection)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading4, detailColumns, textDetail)
import TeamTavern.Client.Components.Missing (missing)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Components.Profile (profileHeader, profileHeading', profileSubheading)
import TeamTavern.Client.Components.Team.ProfileDetails (profileDetails)
import TeamTavern.Client.Pages.Profiles.TeamBadge (communityBadge, partyBadge, platformBadge)
import TeamTavern.Client.Pages.Team.CreateProfileButton (createProfileButton)
import TeamTavern.Client.Pages.Team.Status (Status(..))
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.Size (Size(..))
import TeamTavern.Server.Team.View (Profile, Team)

type ChildSlots children =
    ( games :: Anchor.Slot String
    , createProfile :: H.Slot (Const Void) Void Unit
    | children
    )

profiles
    :: forall action children left
    .  Team
    -> Status
    -> (Profile -> action)
    -> H.ComponentHTML action (ChildSlots children) (Async left)
profiles team @ { profiles: profiles' } status editProfileModalShown =
    card $
    [ cardHeader $
        [ cardHeading "Profiles" ]
        <>
        guard (status == SignedInOwner)
        [ HH.slot (SProxy :: SProxy "createProfile") unit createProfileButton team absurd ]
    ]
    <>
    if Array.null profiles'
    then [ cardSection [ missing
        case status of
        SignedInOwner -> "Your team hasn't created any profiles."
        _ -> "This team hasn't created any profiles." ] ]
    else profiles' <#> \profile -> let
        profileDetails' = profileDetails profile
        about = textDetail profile.about
        ambitions = textDetail profile.ambitions
        in
        cardSection $
        [ profileHeader $
            [ HH.div [ HS.class_ "team-profile-heading-container" ] $
                [ profileHeading' (SProxy :: SProxy "games") profile.handle
                    ("/games/" <> profile.handle <> "/teams") profile.title
                , case profile.size of
                    Party -> partyBadge
                    Community -> communityBadge
                ]
                <> guard (full profile.allPlatforms.tail) (profile.selectedPlatforms # sort <#> platformBadge)
                <> [ profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds ]
            ]
            <>
            guard (status == SignedInOwner)
            [ regularButton "fas fa-user-edit" "Edit profile" $ editProfileModalShown profile ]
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
