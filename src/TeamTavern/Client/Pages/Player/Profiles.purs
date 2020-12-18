module TeamTavern.Client.Pages.Player.Profiles (profiles) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Symbol (SProxy(..))
import Halogen.HTML as HH
import TeamTavern.Client.Components.Button (regularButton)
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading, cardSection)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading4, detailColumns, textDetail)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Missing (missing)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Components.Player.ProfileDetails (profileDetails)
import TeamTavern.Client.Components.Profile (profileHeader, profileHeaderItem, profileHeading, profileSubheading)
import TeamTavern.Client.Pages.Player.CreateProfileButton (createProfileButton)
import TeamTavern.Client.Pages.Player.CreateProfileButton as CreateProfileButton
import TeamTavern.Client.Pages.Player.Status (Status(..))
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Routes.ViewPlayer as ViewPlayer

type ChildSlots children =
    ( games :: NavigationAnchor.Slot String
    , createProfile :: CreateProfileButton.Slot
    | children)

profiles
    :: forall action slots left
    .  ViewPlayer.OkContent
    -> Status
    -> (ViewPlayer.OkContentProfile -> action)
    -> HH.ComponentHTML action (ChildSlots slots) (Async left)
profiles { nickname, profiles: profiles' } status showEditProfileModal =
    card $
    [ cardHeader $
        [ cardHeading "Profiles" ]
        <>
        case status of
        SignedInSelf -> Array.singleton $
            HH.slot (SProxy :: SProxy "createProfile") unit createProfileButton
            { nickname, profileGameHandles: profiles' <#> _.handle } absurd
        _ -> []
    ]
    <>
    if Array.null profiles'
    then [ cardSection [ missing
        case status of
        SignedInSelf -> "You haven't create any profiles."
        _ -> "This player hasn't created any profiles." ] ]
    else profiles' <#> \profile -> let
        profileDetails' = profileDetails profile.fields profile.fieldValues profile.newOrReturning
        ambitions = textDetail profile.ambitions
        in
        cardSection $
        [ profileHeader $
            [ profileHeaderItem $
                [ profileHeading (SProxy :: SProxy "games") profile.handle
                    ("/games/" <> profile.handle <> "/players") profile.title
                ]
                <>
                [ divider
                , profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds
                ]
            ]
            <>
            case status of
            SignedInSelf -> Array.singleton $
                profileHeaderItem
                [ regularButton "fas fa-user-edit" "Edit profile"
                    $ showEditProfileModal profile
                ]
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
