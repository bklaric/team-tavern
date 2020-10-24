module TeamTavern.Client.Pages.Player.Profiles (Slot, profiles) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (intercalate)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen (defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Pages.Player.CreateProfileButton (createProfileButton)
import TeamTavern.Client.Pages.Player.EditPlayerProfile as EditProfile
import TeamTavern.Client.Pages.Player.Types (Nickname, PlayerStatus(..))
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Server.Profile.ViewPlayerProfilesByPlayer.SendResponse as ViewPlayerProfilesByPlayer

data Input = Input Nickname PlayerStatus

data Action
    = Initialize
    | Receive Input
    | ShowModal EditProfile.Input
    | HandleModalOutput (Modal.Output EditProfile.Output)

data State
    = Empty Input
    | Profiles Nickname PlayerStatus ViewPlayerProfilesByPlayer.OkContent

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots =
    ( games :: Anchor.Slot Int
    , createProfile :: H.Slot (Const Void) Void Unit
    , editProfile :: EditProfile.Slot Unit
    )

modalInput
    :: forall other
    .  Nickname
    -> { fieldValues :: Array EditProfile.FieldValue
       , fields :: Array EditProfile.Field
       , handle :: String
       , summary :: Array String
       , title :: String
       , newOrReturning :: Boolean
       | other }
    -> EditProfile.Input
modalInput nickname { handle, title, fields, fieldValues, summary, newOrReturning } =
    { nickname, handle, title, fields, fieldValues, summary, newOrReturning }

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Profiles nickname playerStatus profiles) =
    HH.div [ HS.class_ "card" ] $
    [ HH.h2 [ HS.class_ "card-title" ]
        [ HH.span [ HS.class_ "card-title-text" ]
            [ HH.text "Profiles" ]
        , HH.slot (SProxy :: SProxy "createProfile") unit createProfileButton
            { nickname, profileGameHandles: profiles <#> _.handle } absurd
        ]
    ]
    -- <>
    -- case playerStatus of
    -- SamePlayer -> [ editProfile undefined $ Just <<< HandleModalOutput ]
    -- _ -> []
    <>
    if Array.null profiles
    then Array.singleton $
        HH.div [ HS.class_ "card-section" ]
        [ HH.p_
            [ HH.text
                case playerStatus of
                SamePlayer -> "You haven't created any player profiles."
                _ -> "This player hasn't created any player profiles."
            ]
        ]
    else (profiles # Array.mapWithIndex \index profile ->
        HH.div [ HS.class_ "card-section" ] $
        [ HH.h3 [ HS.class_ "profile-header" ]
            [ HH.div [ HS.class_ "profile-header-item" ] $
                [ navigationAnchorIndexed (SProxy :: SProxy "games") index
                    { path: "/games/" <> profile.handle <> "/players"
                    , content: HH.text profile.title
                    }
                , divider
                , HH.span [ HS.class_ "profile-updated" ]
                    [ HH.text $ "Updated " <> lastUpdated profile.updatedSeconds ]
                ]
            , HH.div [ HS.class_ "profile-header-item" ]
                case playerStatus of
                SamePlayer -> Array.singleton $
                    HH.button
                    [ HS.class_ "regular-button"
                    , HE.onClick $ const $ Just $ ShowModal $ modalInput nickname profile
                    ]
                    [ HH.i [ HP.class_ $ H.ClassName "fas fa-user-edit button-icon" ] []
                    , HH.text "Edit profile"
                    ]
                _ -> []

            ]
        , HH.div [ HS.class_ "profile-columns" ]
            [ HH.div [ HS.class_ "profile-column" ] $
                [ HH.h4 [ HS.class_ "player-profile-section-title" ] [ HH.text "Profile details" ] ]
                <> Array.catMaybes
                (profile.fields <#> \field -> let
                    fieldValue = profile.fieldValues # Array.find \{ fieldKey } -> field.key == fieldKey
                    in
                    case field.ilk, fieldValue of
                    1, Just { url: Just url } -> Just $
                        HH.p [ HS.class_ "profile-field" ]
                        [ HH.i [ HS.class_ $ field.icon <> " profile-field-icon" ] []
                        , HH.a [ HS.class_ "profile-field-url", HP.target "_blank", HP.href url ] [ HH.text field.label ]
                        ]
                    2, Just { optionKey: Just optionKey } ->
                        field.options >>= Array.find (\{ key } -> key == optionKey) <#> \option ->
                            HH.p [ HS.class_ "profile-field" ]
                            [ HH.i [ HS.class_ $ field.icon <> " profile-field-icon" ] []
                            , HH.span [ HS.class_ "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                            , HH.span [ HS.class_ "profile-field-emphasize" ] [ HH.text option.label ]
                            ]
                    3, Just { optionKeys: Just optionKeys } ->
                        case field.options <#> Array.filter \{ key } -> Array.elem key optionKeys of
                        Just fieldOptions | not $ Array.null fieldOptions -> Just $
                            HH.p [ HS.class_ "profile-field" ] $
                            [ HH.i [ HS.class_ $ field.icon <> " profile-field-icon" ] []
                            , HH.span [ HS.class_ "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                            ]
                            <>
                            (intercalate [(HH.text ", ")] $
                                map (\{ label } -> [ HH.span [ HS.class_ "profile-field-emphasize" ] [ HH.text label ] ]) fieldOptions)
                        _ -> Nothing
                    _, _ -> Nothing
                )
                <> (if profile.newOrReturning
                    then Array.singleton $
                        HH.p [ HS.class_ "profile-field" ]
                        [ HH.i [ HS.class_ "fas fa-book profile-field-icon" ] []
                        , HH.span [ HS.class_ "profile-field-labelless" ] [ HH.text "Is a"]
                        , HH.span [ HS.class_ "profile-field-emphasize" ] [ HH.text " new or returning player" ]
                        , HH.text $ " to the game"
                        ]
                    else [])
            , HH.div [ HS.class_ "profile-column" ] $
                (if Array.null $ profile.summary
                then []
                else [ HH.h4 [ HS.class_ "player-profile-section-title" ] [ HH.text "Ambitions" ] ]
                    <> (profile.summary <#> \paragraph ->
                        HH.p [ HS.class_ "profile-summary" ] [ HH.text paragraph ]))
            ]

        ]
    )

loadProfiles :: forall left.
    String -> Async left (Maybe ViewPlayerProfilesByPlayer.OkContent)
loadProfiles nickname = Async.unify do
    response
        <- Fetch.fetch_ ("/api/profiles/by-nickname/" <> nickname <> "/players")
        #  lmap (const Nothing)
    content <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const Nothing)
        _   -> Async.left Nothing
    pure $ Just content

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state of
        Empty (Input nickname playerStatus) -> do
            profiles <- H.lift $ loadProfiles nickname
            case profiles of
                Just profiles' ->
                    H.put $ Profiles nickname playerStatus profiles'
                Nothing -> pure unit
        _ -> pure unit
handleAction (Receive (Input nickname playerStatus)) = do
    state <- H.get
    case state of
        Profiles currentNickname _ _ | nickname == currentNickname -> pure unit
        _ -> do
            profiles <- H.lift $ loadProfiles nickname
            case profiles of
                Just profiles' -> H.put $ Profiles nickname playerStatus profiles'
                Nothing -> pure unit
handleAction (ShowModal profile) =
    -- Modal.showWith profile (SProxy :: SProxy "editProfile")
    pure unit
handleAction (HandleModalOutput output) = do
    -- Modal.hide (SProxy :: SProxy "editProfile")
    case output of
        Modal.OutputRaised (EditProfile.ProfileUpdated _) -> do
            state <- H.get
            case state of
                Profiles nickname status _ -> do
                    profiles <- H.lift $ loadProfiles nickname
                    case profiles of
                        Just profiles' -> H.put $ Profiles nickname status profiles'
                        Nothing -> pure unit
                _ -> pure unit
        _ -> pure unit

component :: forall output left query.
    H.Component HH.HTML query Input output (Async left)
component = mkComponent
    { initialState: Empty
    , render
    , eval: mkEval $ defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

profiles
    :: forall children action left
    .  Nickname
    -> PlayerStatus
    -> HH.ComponentHTML action (profiles :: Slot | children) (Async left)
profiles nickname playerStatus =
    HH.slot (SProxy :: SProxy "profiles") unit component (Input nickname playerStatus) absurd
