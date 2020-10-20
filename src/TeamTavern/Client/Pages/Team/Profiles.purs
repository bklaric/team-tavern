module TeamTavern.Client.Pages.Team.Profiles (profiles) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import TeamTavern.Client.Components.Detail (textDetail)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Components.Team.ProfileDetail (profileDetails)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Client.Team.CreateProfileButton (createProfileButton)
import TeamTavern.Server.Team.View (Profile)

type ChildSlots children =
    ( games :: Anchor.Slot String
    , createProfile :: H.Slot (Const Void) Void Unit
    | children)

profileAmbitionsColumn :: forall slots action. Profile -> Array (HH.HTML slots action)
profileAmbitionsColumn profile | Array.null profile.summary = []
profileAmbitionsColumn profile = Array.singleton $
    HH.div [ HS.class_ "profile-column" ] $
    [ HH.h4 [ HS.class_ "player-profile-section-title" ] [ HH.text "Ambitions" ] ]
    <> maybe [] identity (textDetail profile.summary)

profileDetailsColumn :: forall slots action. Profile -> Array (HH.HTML slots action)
profileDetailsColumn profile = let
    details = profileDetails profile.fields profile.fieldValues profile.newOrReturning
    in
    if Array.null details
    then []
    else Array.singleton $
        HH.div [ HS.class_ "profile-column" ] $
        [ HH.h4 [ HS.class_ "player-profile-section-title" ] [ HH.text "Profile details" ] ]
        <> details

profiles
    :: forall action children left
    .  String
    -> Array Profile
    -> (Profile -> action)
    -> H.ComponentHTML action (ChildSlots children) (Async left)
profiles teamHandle profiles' showEditProfileModal =
    HH.div [ HS.class_ "card" ] $
    [ HH.h2 [ HS.class_ "card-title" ]
        [ HH.span [ HS.class_ "card-title-text" ]
            [ HH.text "Profiles" ]
        , HH.slot (SProxy :: SProxy "createProfile") unit createProfileButton
            { teamHandle, profileGameHandles: profiles' <#> _.handle } absurd
        ]
    ]
    <>
    if Array.null profiles'
    then Array.singleton $
        HH.div [ HS.class_ "card-section" ]
        [ HH.p_ [ HH.text "No profiles, kek." ] ]
    else profiles' <#> \profile ->
        HH.div [ HS.class_ "card-section" ] $
        [ HH.h3 [ HS.class_ "profile-header" ]
            [ HH.div [ HS.class_ "profile-header-item" ] $
                [ navigationAnchorIndexed (SProxy :: SProxy "games") profile.handle
                    { path: "/games/" <> profile.handle <> "/teams"
                    , content: HH.text profile.title
                    }
                ]
                <>
                [ divider
                , HH.span [ HS.class_ "profile-updated" ]
                    [ HH.text $ "Updated " <> lastUpdated profile.updatedSeconds ]
                ]
            , HH.div [ HS.class_ "profile-header-item" ] $
                [ HH.button
                    [ HS.class_ "regular-button"
                    , HE.onClick $ const $ Just $ showEditProfileModal profile
                    ]
                    [ HH.i [ HS.class_ "fas fa-user-edit button-icon" ] []
                    , HH.text "Edit profile"
                    ]
                ]
                <>
                [

                ]
            ]
        , HH.div [ HS.class_ "profile-columns" ] $
            profileDetailsColumn profile <> profileAmbitionsColumn profile
        ]
