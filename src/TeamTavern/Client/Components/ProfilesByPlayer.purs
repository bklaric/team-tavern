module TeamTavern.Client.Components.ProfilesByPlayer
    (Slot, profilesByPlayer) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (intercalate)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Foldable (find)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen (ClassName(..), defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.EditProfile (ProfileIlk(..), editProfile)
import TeamTavern.Client.EditProfile as EditProfile
import TeamTavern.Client.Script.Cookie (getPlayerNickname)
import TeamTavern.Server.Profile.ViewByPlayer.SendResponse as ViewByPlayer
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Nickname = String

data Input = Input Nickname ProfileIlk

data Action
    = Init
    | Receive Input
    | ShowEditProfileModal EditProfile.Input MouseEvent
    | HandleEditProfileMessage (Modal.Message EditProfile.Message)

data State
    = Empty Input
    | Profiles ViewByPlayer.OkContent (Maybe Nickname) ProfileIlk

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( games :: Anchor.Slot Int
    , editProfile :: EditProfile.Slot Unit
    )

yearSeconds :: Number
yearSeconds = 60.0 * 60.0 * 24.0 * 365.0

monthSeconds :: Number
monthSeconds = 60.0 * 60.0 * 24.0 * 30.0

daySeconds :: Number
daySeconds = 60.0 * 60.0 * 24.0

hourSeconds :: Number
hourSeconds = 60.0 * 60.0

minuteSeconds :: Number
minuteSeconds = 60.0

lastUpdated :: Number -> String
lastUpdated updatedSeconds = let
    yearsAgo = floor(updatedSeconds / yearSeconds)
    monthsAgo = floor(updatedSeconds / monthSeconds)
    daysAgo = floor(updatedSeconds / daySeconds)
    hoursAgo = floor(updatedSeconds / hourSeconds)
    minutesAgo = floor(updatedSeconds / minuteSeconds)
    interval =
        if yearsAgo > 0 then Just { unit: "year", count: yearsAgo } else
        if monthsAgo > 0 then Just { unit: "month", count: monthsAgo } else
        if daysAgo > 0 then Just { unit: "day", count: daysAgo } else
        if hoursAgo > 0 then Just { unit: "hour", count: hoursAgo } else
        if minutesAgo > 0 then Just { unit: "minute", count: minutesAgo } else
        Nothing
    in
    case interval of
    Just { unit, count } -> show count <> " " <> unit <> if count == 1 then "" else "s" <> " ago"
    Nothing -> "less than a minute ago"

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Profiles profiles nickname' profileIlk) =
    HH.div [ HP.class_ $ HH.ClassName "card" ] $
    [ HH.h2 [ HP.class_ $ HH.ClassName "card-title" ] [ HH.text
        case profileIlk of
        Players -> "Player profiles"
        Teams -> "Team profiles" ] ]
    <> case nickname' of
        Nothing -> []
        Just _ -> [ editProfile $ Just <<< HandleEditProfileMessage ]
    <>
    if Array.null profiles
    then pure $
        HH.div [ HP.class_ $ ClassName "card-section" ]
        [ HH.p_ [ HH.text
            case { nickname: nickname', profileIlk } of
            { nickname: Just nickname, profileIlk: Players } -> "You haven't created any player profiles."
            { nickname: Just nickname, profileIlk: Teams } -> "You haven't created any team profiles."
            { nickname: Nothing, profileIlk: Players } -> "This player hasn't created any player profiles."
            { nickname: Nothing, profileIlk: Teams } -> "This player hasn't created any team profiles." ] ]
    else
        (profiles # mapWithIndex \index { handle, title, summary, fieldValues, fields, updated, updatedSeconds } ->
            HH.div [ HP.class_ $ ClassName "card-section" ] $ join
            [ pure $
                HH.h3 [ HP.class_ $ ClassName "profile-title" ] $ join
                    [ pure $ navigationAnchorIndexed (SProxy :: SProxy "games") index
                        { path: "/games/" <> handle <>
                            case profileIlk of
                            Players -> "/players"
                            Teams -> "/teams"
                        , content: HH.text title
                        }
                    , case nickname' of
                        Nothing -> []
                        Just nickname -> pure $
                            HH.button
                                [ HP.class_ $ HH.ClassName "regular-button title-button"
                                , HE.onClick $ Just <<< ShowEditProfileModal
                                    { nickname, handle, title, profileIlk, summary, fieldValues, fields }
                                ]
                                [ HH.i [ HP.class_ $ H.ClassName "fas fa-user-edit button-icon" ] []
                                , HH.text
                                    case profileIlk of
                                    Players -> "Edit player profile"
                                    Teams -> "Edit team profile"
                                ]
                    , pure $ divider
                    , pure $ HH.span [ HP.class_ $ HH.ClassName "profile-updated" ]
                        [ HH.text $ "Updated " <> lastUpdated updatedSeconds ]
                    ]
            , Array.catMaybes $ fields <#> \field -> let
                fieldValue = fieldValues # find \ { fieldKey } -> field.key == fieldKey
                in
                case { profileIlk, type: field.type, fieldValue } of
                { profileIlk: Players, type: 1, fieldValue: Just { url: Just url' } } -> Just $
                    HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                    [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                    , HH.a [ HP.class_ $ HH.ClassName "profile-field-label", HP.href url' ] [ HH.text field.label ]
                    ]
                { profileIlk: Players, type: 2, fieldValue: Just { optionKey: Just optionKey' } } -> let
                    option' = field.options >>= find (\{ key } -> key == optionKey')
                    in
                    option' <#> \{ option } ->
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                        , HH.text option
                        ]
                { profileIlk: Teams, type: 2, fieldValue: Just { optionKeys: Just optionKeys' } } -> let
                    fieldOptions' = field.options <#> Array.filter \{ key } -> Array.elem key optionKeys'
                    in
                    case fieldOptions' of
                    Just fieldOptions | not $ Array.null fieldOptions -> Just $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                        , HH.text $ intercalate ", " (fieldOptions <#> _.option)
                        ]
                    _ -> Nothing
                { profileIlk: Players, type: 3, fieldValue: Just { optionKeys: Just optionKeys' } } -> let
                    fieldOptions' = field.options <#> Array.filter \{ key } -> Array.elem key optionKeys'
                    in
                    case fieldOptions' of
                    Just fieldOptions | not $ Array.null fieldOptions -> Just $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                        , HH.text $ intercalate ", " (fieldOptions <#> _.option)
                        ]
                    _ -> Nothing
                { profileIlk: Teams, type: 3, fieldValue: Just { optionKeys: Just optionKeys' } } -> let
                    fieldOptions' = field.options <#> Array.filter \{ key } -> Array.elem key optionKeys'
                    in
                    case fieldOptions' of
                    Just fieldOptions | not $ Array.null fieldOptions -> Just $
                        HH.p [ HP.class_ $ HH.ClassName "profile-field" ]
                        [ HH.i [ HP.class_ $ HH.ClassName $ field.icon <> " profile-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "profile-field-label" ] [ HH.text $ field.label <> ": " ]
                        , HH.text $ intercalate ", " (fieldOptions <#> _.option)
                        ]
                    _ -> Nothing
                _ ->  Nothing
            , summary <#> \paragraph -> HH.p_ [ HH.text paragraph ]
            ])

loadProfiles :: forall left. Nickname -> ProfileIlk -> Async left State
loadProfiles nickname profileIlk = Async.unify do
    let emptyState = Empty $ Input nickname profileIlk
    response <-  Fetch.fetch_ ("/api/profiles/by-nickname/" <> nickname <> "?ilk=" <>
        case profileIlk of
        Players -> "1"
        Teams -> "2")
        # lmap (const emptyState)
    content <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const emptyState)
        _ -> Async.left emptyState
    cookieNickname <- H.liftEffect getPlayerNickname
    pure $ Profiles content
        (case cookieNickname of
        Just cookieNickname' | cookieNickname' == nickname -> Just nickname
        _ -> Nothing)
        profileIlk

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    state <- H.get
    case state of
        Empty (Input nickname profileIlk) -> do
            state' <- H.lift $ loadProfiles nickname profileIlk
            H.put state'
        _ -> pure unit
handleAction (Receive (Input nickname profileIlk)) = do
    state <- H.lift $ loadProfiles nickname profileIlk
    H.put state
handleAction (ShowEditProfileModal profile event) = do
    H.liftEffect $ preventDefault $ toEvent event
    Modal.showWith profile (SProxy :: SProxy "editProfile")
handleAction (HandleEditProfileMessage message) = do
    Modal.hide (SProxy :: SProxy "editProfile")
    case message of
        Modal.Inner (EditProfile.ProfileUpdated nickname profileIlk) ->
            handleAction $ Receive $ Input nickname profileIlk
        _ -> pure unit

component :: forall output left query.
    H.Component HH.HTML query Input output (Async left)
component = mkComponent
    { initialState: Empty
    , render
    , eval: mkEval $ defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , receive = Just <<< Receive
        }
    }

profilesByPlayer
    :: forall query children left
    .  String
    -> ProfileIlk
    -> HH.ComponentHTML
        query (profilesByPlayer :: Slot Unit | children) (Async left)
profilesByPlayer nickname profileIlk = HH.slot
    (SProxy :: SProxy "profilesByPlayer") unit
    component (Input nickname profileIlk) absurd
