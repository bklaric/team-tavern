module TeamTavern.Client.Pages.Player.Profiles (profiles) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Array.Extra (full)
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import Halogen.HTML as HH
import TeamTavern.Client.Components.Button (regularButton)
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading, cardSection)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading4, detailColumns, textDetail)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Missing (missing)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Components.Player.ProfileDetails (PlatformIdSlots, profileDetails)
import TeamTavern.Client.Components.Profile (profileHeader, profileHeading', profileSubheading)
import TeamTavern.Client.Pages.Player.CreateProfileButton (createProfileButton)
import TeamTavern.Client.Pages.Player.CreateProfileButton as CreateProfileButton
import TeamTavern.Client.Pages.Player.Status (Status(..))
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Routes.ViewPlayer as ViewPlayer

type ChildSlots children = PlatformIdSlots
    ( games :: NavigationAnchor.Slot String
    , createProfile :: CreateProfileButton.Slot
    | children)

profiles
    :: forall action slots left
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
        [ HH.slot (SProxy :: SProxy "createProfile") unit createProfileButton player absurd ]
    ]
    <>
    if Array.null profiles'
    then [ cardSection [ missing
        case status of
        SignedInSelf -> "You haven't create any profiles."
        _ -> "This player hasn't created any profiles." ] ]
    else profiles' <#> \profile -> let
        profileDetails' = profileDetails profile.platform profile.fields profile.fieldValues profile.newOrReturning
        about = textDetail profile.about
        in
        cardSection $
        [ profileHeader $
            [ HH.div_ $
                [ profileHeading' (SProxy :: SProxy "games") profile.handle
                    ("/games/" <> profile.handle <> "/players") profile.title
                ]
                <>
                [ divider
                , profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds
                ]
            ]
            <>
            guard (status == SignedInSelf)
            [ regularButton "fas fa-user-edit" "Edit profile" $ showEditProfileModal profile ]
        ]
        <>
        guard (full profileDetails' || full about)
        [ detailColumns $
            guard (full profileDetails')
            [ detailColumn $ [ detailColumnHeading4 "Details" ] <> profileDetails' ]
            <>
            guard (full about)
            [ detailColumn $ [ detailColumnHeading4 "About" ] <> about ]
        ]
