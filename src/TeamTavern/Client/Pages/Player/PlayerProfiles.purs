module TeamTavern.Client.Pages.Player.PlayerProfiles
    (Slot, playerProfiles) where

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
import Data.Symbol (class IsSymbol, SProxy(..))
import Halogen (defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Pages.Player.EditPlayerProfile (editProfile)
import TeamTavern.Client.Pages.Player.EditPlayerProfile as EditProfile
import TeamTavern.Client.Pages.Player.Types (Nickname, PlayerStatus(..))
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Server.Profile.ViewPlayerProfilesByPlayer.SendResponse as ViewPlayerProfilesByPlayer
import Undefined (undefined)

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
    HH.div [ HP.class_ $ HH.ClassName "card" ] $
    [ HH.h2 [ HP.class_ $ HH.ClassName "card-title" ]
        [ HH.text "Profiles" ]
    ]
    -- <>
    -- case playerStatus of
    -- SamePlayer -> [ editProfile undefined $ Just <<< HandleModalOutput ]
    -- _ -> []
    <>
    if Array.null profiles
    then Array.singleton $
        HH.div [ HP.class_ $ HH.ClassName "card-section" ]
        [ HH.p_
            [ HH.text
                case playerStatus of
                SamePlayer -> "You haven't created any player profiles."
                _ -> "This player hasn't created any player profiles."
            ]
        ]
    else (profiles # Array.mapWithIndex \index profile ->
        HH.div [ HP.class_ $ HH.ClassName "card-section" ] $
        [ HH.h3 [ HP.class_ $ HH.ClassName "profile-header" ]
            [ HH.div [ HP.class_ $ HH.ClassName "profile-header-item" ] $
                [ navigationAnchorIndexed (SProxy :: SProxy "games") index
                    { path: "/games/" <> profile.handle <> "/players"
                    , content: HH.text profile.title
                    }
                , divider
                , HH.span [ HP.class_ $ HH.ClassName "profile-updated" ]
                    [ HH.text $ "Updated " <> lastUpdated profile.updatedSeconds ]
                ]
            , HH.div [ HP.class_ $ HH.ClassName "profile-header-item" ]
                case playerStatus of
                SamePlayer -> Array.singleton $
                    HH.button
                    [ HP.class_ $ HH.ClassName "regular-button"
                    , HE.onClick $ const $ Just $ ShowModal $ modalInput nickname profile
                    ]
                    [ HH.i [ HP.class_ $ H.ClassName "fas fa-user-edit button-icon" ] []
                    , HH.text "Edit profile"
                    ]
                _ -> []

            ]
        , HH.div [ HP.class_ $ HH.ClassName "profile-columns" ]
            [ HH.div [ HP.class_ $ HH.ClassName "profile-column" ] $
                [ HH.h4 [ HP.class_ $ HH.ClassName "player-profile-section-title" ] [ HH.text "Profile details" ] ]
                <> Array.catMaybes
                (profile.fields <#> \field -> let
                    fieldValue = profile.fieldValues # Array.find \{ fieldKey } -> field.key == fieldKey
                    in
                    case field.ilk, fieldValue of
                    1, Just { url: Just url } -> Just $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                        , HH.a [ HP.class_ $ HH.ClassName "profile-field-url", HP.target "_blank", HP.href url ] [ HH.text field.label ]
                        ]
                    2, Just { optionKey: Just optionKey } ->
                        field.options >>= Array.find (\{ key } -> key == optionKey) <#> \option ->
                            HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                            [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                            , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                            , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text option.label ]
                            ]
                    3, Just { optionKeys: Just optionKeys } ->
                        case field.options <#> Array.filter \{ key } -> Array.elem key optionKeys of
                        Just fieldOptions | not $ Array.null fieldOptions -> Just $
                            HH.p [ HP.class_ $ HH.ClassName "profile-field" ] $
                            [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                            , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                            ]
                            <>
                            (intercalate [(HH.text ", ")] $
                                map (\{ label } -> [ HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text label ] ]) fieldOptions)
                        _ -> Nothing
                    _, _ -> Nothing
                )
                <> (if profile.newOrReturning
                    then Array.singleton $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-book profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-labelless" ] [ HH.text "Is a"]
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-emphasize" ] [ HH.text " new or returning player" ]
                        , HH.text $ " to the game"
                        ]
                    else [])
            , HH.div [ HP.class_ $ HH.ClassName "profile-column" ] $
                (if Array.null $ profile.summary
                then []
                else [ HH.h4 [ HP.class_ $ HH.ClassName "player-profile-section-title" ] [ HH.text "Ambitions" ] ]
                    <> (profile.summary <#> \paragraph ->
                        HH.p [ HP.class_ $ HH.ClassName "profile-summary" ] [ HH.text paragraph ]))
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

playerProfiles
    :: forall children' name children action left
    .  Cons name (Slot) children' children
    => IsSymbol name
    => Nickname
    -> PlayerStatus
    -> SProxy name
    -> HH.ComponentHTML action children (Async left)
playerProfiles nickname playerStatus slot =
    HH.slot slot unit component (Input nickname playerStatus) absurd
