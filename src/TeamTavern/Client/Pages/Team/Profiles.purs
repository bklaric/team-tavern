module TeamTavern.Client.Pages.Team.Profiles (profiles) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
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
import TeamTavern.Client.Pages.Profiles.TeamBadge (communityBadge, partyBadge)
import TeamTavern.Client.Pages.Team.CreateProfileButton (createProfileButton)
import TeamTavern.Client.Pages.Team.Status (Status(..))
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.Size (Size(..))
import TeamTavern.Server.Team.View (Profile)

type ChildSlots children =
    ( games :: Anchor.Slot String
    , createProfile :: H.Slot (Const Void) Void Unit
    | children
    )

profiles
    :: forall action children left
    .  String
    -> Array Profile
    -> Status
    -> (Profile -> action)
    -> H.ComponentHTML action (ChildSlots children) (Async left)
profiles teamHandle profiles' status editProfileModalShown =
    card $
    [ cardHeader $
        [ cardHeading "Profiles" ]
        <>
        case status of
        SignedInOwner -> Array.singleton $
            HH.slot (SProxy :: SProxy "createProfile") unit createProfileButton
            { teamHandle, profileGameHandles: profiles' <#> _.handle } absurd
        _ -> []
    ]
    <>
    if Array.null profiles'
    then [ cardSection [ missing
        case status of
        SignedInOwner -> "Your team hasn't created any profiles."
        _ -> "This team hasn't created any profiles." ] ]
    else profiles' <#> \profile -> let
        profileDetails' = profileDetails profile
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
                , profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds
                ]
            ]
            <>
            case status of
            SignedInOwner -> Array.singleton $
                regularButton "fas fa-user-edit" "Edit profile" $ editProfileModalShown profile
            _ -> []
        ]
        <>
        if Array.null profileDetails' && Array.null ambitions
        then []
        else Array.singleton $ detailColumns $
            ( if Array.null profileDetails'
                then []
                else Array.singleton $ detailColumn $
                    [ detailColumnHeading4 "Details" ] <> profileDetails'
            )
            <>
            ( if Array.null ambitions
                then []
                else Array.singleton $ detailColumn $
                    [ detailColumnHeading4 "Ambitions" ] <> ambitions
            )
