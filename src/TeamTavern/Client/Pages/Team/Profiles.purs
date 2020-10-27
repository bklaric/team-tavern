module TeamTavern.Client.Pages.Team.Profiles (profiles) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Button (regularIconButton)
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading, cardSection)
import TeamTavern.Client.Components.Detail (detailColumn, detailColumnHeading, detailColumns, textDetail)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Components.Profile (profileHeader, profileHeaderItem, profileSubheading)
import TeamTavern.Client.Components.Team.ProfileDetails (profileDetails)
import TeamTavern.Client.Pages.Team.CreateProfileButton (createProfileButton)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Server.Team.View (Profile)

type ChildSlots children =
    ( games :: Anchor.Slot String
    , createProfile :: H.Slot (Const Void) Void Unit
    | children)

profiles
    :: forall action children left
    .  String
    -> Array Profile
    -> (Profile -> action)
    -> H.ComponentHTML action (ChildSlots children) (Async left)
profiles teamHandle profiles' editProfileModalShown =
    card $
    [ cardHeader
        [ cardHeading "Profiles"
        , HH.slot (SProxy :: SProxy "createProfile") unit createProfileButton
            { teamHandle, profileGameHandles: profiles' <#> _.handle } absurd
        ]
    ]
    <>
    if Array.null profiles'
    then [ cardSection [ HH.p_ [ HH.text "No profiles, kek." ] ] ]
    else profiles' <#> \profile -> let
        profileDetails' = profileDetails profile.fields profile.fieldValues profile.newOrReturning
        ambitions = textDetail profile.summary
        in
        cardSection $
        [ profileHeader
            [ profileHeaderItem $
                [ navigationAnchorIndexed (SProxy :: SProxy "games") profile.handle
                    { path: "/games/" <> profile.handle <> "/teams"
                    , content: HH.text profile.title
                    }
                ]
                <>
                [ divider
                , profileSubheading $ "Updated " <> lastUpdated profile.updatedSeconds
                ]
            , profileHeaderItem
                [ regularIconButton "fas fa-user-edit" "Edit profile"
                    $ editProfileModalShown profile
                ]
            ]
        ]
        <>
        if Array.null profileDetails' && Array.null ambitions
        then []
        else Array.singleton $ detailColumns $
            ( if Array.null profileDetails'
                then []
                else Array.singleton $ detailColumn $
                    [ detailColumnHeading "Details" ] <> profileDetails'
            )
            <>
            ( if Array.null ambitions
                then []
                else Array.singleton $ detailColumn $
                    [ detailColumnHeading "Ambitions" ] <> ambitions
            )
