module TeamTavern.Client.Pages.Team.Profiles (profiles) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Client.Team.CreateProfileButton (createProfileButton)
import TeamTavern.Server.Team.View (Profile)

type ChildSlots children =
    ( games :: Anchor.Slot String
    , createProfile :: H.Slot (Const Void) Void Unit
    | children)

profileDetails :: forall fieldOptionFields fieldValueFields fieldFields slots action.
    Array
        { icon :: String
        , key :: String
        , label :: String
        , options :: Array
            { key :: String
            , label :: String
            | fieldOptionFields }
        | fieldFields
        }
    -> Array
        { fieldKey :: String
        , optionKeys :: Array String
        | fieldValueFields
        }
    -> Boolean
    -> Array (HH.HTML slots action)
profileDetails fields fieldValues newOrReturning =
    (fields
    <#> (\field ->
        case fieldValues # Array.find \{ fieldKey } -> field.key == fieldKey of
        Just { optionKeys } -> let
            fieldOptions = field.options # Array.filter \{ key } -> Array.elem key optionKeys
            in
            if not $ Array.null fieldOptions
            then Just $
                HH.p [ HS.class_ "profile-field" ] $
                [ HH.i [ HS.class_ $ field.icon <> " profile-field-icon" ] []
                , HH.span [ HS.class_ "profile-field-label" ]
                    [ HH.text $ field.label <> ": " ]
                ]
                <>
                (Array.intercalate [(HH.text ", ")] $
                    map
                    (\{ label } ->
                        [ HH.span [ HS.class_ "profile-field-emphasize" ]
                            [ HH.text label ]
                        ])
                    fieldOptions)
            else Nothing
        _ -> Nothing)
    # Array.catMaybes)
    <> (if newOrReturning
        then Array.singleton $
            HH.p [ HS.class_ "profile-field" ]
            [ HH.i [ HS.class_ "fas fa-book profile-field-icon" ] []
            , HH.span [ HS.class_ "profile-field-labelless" ] [ HH.text "Are"]
            , HH.span [ HS.class_ "profile-field-emphasize" ] [ HH.text " new or returning players" ]
            , HH.text $ " to the game"
            ]
        else [])

profileAmbitionsColumn :: forall slots action. Profile -> Array (HH.HTML slots action)
profileAmbitionsColumn profile | Array.null profile.summary = []
profileAmbitionsColumn profile = Array.singleton $
    HH.div [ HS.class_ "profile-column" ] $
    [ HH.h4 [ HS.class_ "player-profile-section-title" ] [ HH.text "Ambitions" ] ]
    <> (profile.summary <#> \paragraph ->
        HH.p [ HS.class_ "profile-summary" ] [ HH.text paragraph ])

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
    -> H.ComponentHTML action (ChildSlots children) (Async left)
profiles teamHandle profiles' =
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
        [ HH.h3 [ HS.class_ "player-profile-title" ]
            [ HH.div [ HS.class_ "player-profile-title-item" ] $
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
            ]
        , HH.div [ HS.class_ "profile-columns" ] $
            profileDetailsColumn profile <> profileAmbitionsColumn profile
        ]
