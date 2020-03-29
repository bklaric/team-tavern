module TeamTavern.Client.Components.Account.EditDetails
    (Message(..), Slot, editDetails) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (sortBy)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.Foldable (any, find, foldl)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Options ((:=))
import Data.String (trim)
import Data.String as String
import Data.Traversable (traverse)
import Data.Variant (SProxy(..), match)
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.CloseButton (closeButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.SelectImperative.MultiSelect (multiSelect)
import TeamTavern.Client.Components.SelectImperative.MultiSelect as MultiSelect
import TeamTavern.Client.Components.SelectImperative.SingleSelect (singleSelect, singleSelect')
import TeamTavern.Client.Components.SelectImperative.SingleSelect as SingleSelect
import TeamTavern.Client.Script.Cookie (getPlayerInfo)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Infrastructure.Countries (allCountries)
import TeamTavern.Server.Infrastructure.Languages (allLanguages)
import TeamTavern.Server.Infrastructure.Timezones (Timezone, allTimezones)
import TeamTavern.Server.Player.Update.SendResponse as Update
import TeamTavern.Server.Player.ViewAccount.SendResponse as ViewAccount
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLInputElement as HTMLInputElement

data Action
    = Init
    | TimezoneInput LoadedState (Maybe Timezone)
    | WeekdayFromInput String
    | WeekdayToInput String
    | WeekendFromInput String
    | WeekendToInput String
    | Update LoadedState Event
    | Close

data Message = DetailsEditted String | CloseClicked

type LoadedState =
    { nickname :: String
    , timezoneSet :: Boolean
    , weekdayFrom :: String
    , weekdayTo :: String
    , weekendFrom :: String
    , weekendTo :: String
    , discordTagError :: Boolean
    , otherError :: Boolean
    , submitting :: Boolean
    }

data State
    = Empty
    | Error
    | Loaded LoadedState

type ChildSlots =
    ( languageInput :: MultiSelect.Slot String
    , countryInput :: SingleSelect.Slot String
    , timezoneInput :: SingleSelect.Slot Timezone
    )

type Slot = H.Slot (Modal.Query Unit (Const Void)) (Modal.Message Message) Unit

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] []
render Error = HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] []
render (Loaded state @ { timezoneSet, discordTagError, otherError, submitting }) =
    HH.div [ HP.class_ $ HH.ClassName "wide-single-form-container" ] $ pure $
    HH.form
    [ HP.class_ $ H.ClassName "form", HE.onSubmit $ Just <<< Update state ]
    [ closeButton Close
    , HH.h2  [ HP.class_ $ HH.ClassName "form-heading" ]
        [ HH.text "Edit player details" ]
    , HH.div [ HP.class_ $ HH.ClassName "responsive-input-groups" ]
        [ HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label"]
                [ HH.text "Discord tag" ]
            , HH.input
                [ HP.ref $ H.RefLabel "discord-tag"
                , HP.class_ $ HH.ClassName "text-line-input"
                , HP.type_ HP.InputText
                ]
            , HH.label
                [ HP.class_ $ HH.ClassName "input-underlabel" ]
                [ HH.text "Example: username#1234" ]
            , HH.p
                [ HP.class_ $ inputErrorClass discordTagError ]
                [ HH.text $ "This does not look like a valid Discord tag." ]
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.text "Birthday" ]
            , HH.input
                [ HP.ref $ H.RefLabel "birthday"
                , HP.class_ $ HH.ClassName "text-line-input"
                , HP.type_ HP.InputDate
                ]
            , HH.label
                [ HP.class_ $ HH.ClassName "input-underlabel" ]
                [ HH.text $ "Your birthday will be used to calculate your age "
                    <> "and will not be shown to anyone."
                ]
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.text "Language" ]
            , multiSelect (SProxy :: SProxy "languageInput")
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.text "Country" ]
            , singleSelect (SProxy :: SProxy "countryInput")
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.text "Timezone" ]
            , singleSelect' (SProxy :: SProxy "timezoneInput")
                (\(SingleSelect.SelectedChanged option) ->
                    Just $ TimezoneInput state option)
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ] $
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.text "Online on weekdays" ]
            , HH.div [ HP.class_ $ HH.ClassName "timespan-group" ]
                [ HH.span [ HP.class_ $ HH.ClassName "timespan-group-from" ] [ HH.text "From" ]
                , HH.input
                    [ HP.id_ "weekday-start"
                    , HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                    , HP.ref $ H.RefLabel "weekday-start"
                    , HP.type_ HP.InputTime
                    , HP.disabled $ not timezoneSet
                    , HE.onValueChange $ Just <<< WeekdayFromInput
                    ]
                , HH.span [ HP.class_ $ HH.ClassName "timespan-group-to" ] [ HH.text "to" ]
                , HH.input
                    [ HP.id_ "weekday-end"
                    , HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                    , HP.ref $ H.RefLabel "weekday-end"
                    , HP.type_ HP.InputTime
                    , HP.disabled $ not timezoneSet
                    , HE.onValueChange $ Just <<< WeekdayToInput
                    ]
               ]
            ]
            <>
            (if not timezoneSet
            then Array.singleton $
                HH.label
                    [ HP.class_ $ HH.ClassName "input-underlabel" ]
                    [ HH.text $ "Set your timezone to unlock this field." ]
            else if String.null state.weekdayFrom && (not $ String.null state.weekdayTo)
                || (not $ String.null state.weekdayFrom) && String.null state.weekdayTo
            then Array.singleton $
                HH.label
                [ HP.class_ $ HH.ClassName "input-underlabel" ]
                [ HH.text $ "Enter both times for the field to have effect."
                ]
            else [])
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ] $
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.text "Online on weekends" ]
            , HH.div [ HP.class_ $ HH.ClassName "timespan-group" ]
                [ HH.span [ HP.class_ $ HH.ClassName "timespan-group-from" ] [ HH.text "From" ]
                , HH.input
                    [ HP.id_ "weekend-start"
                    , HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                    , HP.ref $ H.RefLabel "weekend-start"
                    , HP.type_ HP.InputTime
                    , HP.disabled $ not timezoneSet
                    , HE.onValueChange $ Just <<< WeekendFromInput
                    ]
                , HH.span [ HP.class_ $ HH.ClassName "timespan-group-to" ] [ HH.text "to" ]
                , HH.input
                    [ HP.id_ "weekend-end"
                    , HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                    , HP.ref $ H.RefLabel "weekend-end"
                    , HP.type_ HP.InputTime
                    , HP.disabled $ not timezoneSet
                    , HE.onValueChange $ Just <<< WeekendToInput
                    ]
                ]
            ]
            <>
            (if not timezoneSet
            then Array.singleton $
                HH.label
                    [ HP.class_ $ HH.ClassName "input-underlabel" ]
                    [ HH.text $ "Set your timezone to unlock this field." ]
            else if String.null state.weekendFrom && (not $ String.null state.weekendTo)
                || (not $ String.null state.weekendFrom) && String.null state.weekendTo
            then Array.singleton $
                HH.label
                [ HP.class_ $ HH.ClassName "input-underlabel" ]
                [ HH.text $ "Enter both times for the field to have effect."
                ]
            else [])
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.text "Microphone" ]
            , HH.label
                [ HP.class_ $ HH.ClassName "checkbox-input-label" ]
                [ HH.input
                    [ HP.ref $ H.RefLabel "has-microphone"
                    , HP.class_ $ HH.ClassName "checkbox-input"
                    , HP.type_ HP.InputCheckbox
                    ]
                , HH.text "I have a microphone and I'm willing to communicate."
                ]
            ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "form-submit-button"
        , HP.disabled submitting
        ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-edit button-icon" ] []
        , HH.text
            if submitting
            then "Editing details..."
            else "Edit details"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

loadDetails :: forall left. String -> Async left (Maybe ViewAccount.OkContent)
loadDetails nickname = Async.unify do
    response <-
        Fetch.fetch
        ("/api/players/by-nickname/" <> nickname <> "/details")
        (Fetch.credentials := Fetch.Include)
        # lmap (const Nothing)
    content :: ViewAccount.OkContent <- case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing
    pure $ Just content

editDetails' :: forall left.
    LoadedState -> ViewAccount.OkContent -> Async left (Maybe LoadedState)
editDetails' state details = Async.unify do
    response <-
        Fetch.fetch
        ("/api/players/by-nickname/" <> state.nickname <> "/details")
        (  Fetch.method := PUT
        <> Fetch.body := Json.writeJSON
            { discordTag: details.discordTag
            , birthday: details.birthday
            , languages: details.languages
            , country: details.country
            , timezone: details.timezone
            , weekdayStart: details.weekdayStart
            , weekdayEnd: details.weekdayEnd
            , weekendStart: details.weekendStart
            , weekendEnd: details.weekendEnd
            , hasMicrophone: details.hasMicrophone
            }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true})
                (\(error :: Update.BadRequestContent) -> Just $ match
                    { invalidModel:
                        foldl (\state' -> match
                        { invalidDiscordTag:
                            const $ state' { discordTagError = true }
                        })
                        state
                    , nicknameTaken: const $ state { discordTagError = true }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

getValue :: forall state action slots message monad. MonadEffect monad =>
    H.RefLabel -> H.HalogenM state action slots message monad (Maybe String)
getValue label = do
    input <- H.getRef label
    input
        >>= HTMLInputElement.fromElement
        # traverse HTMLInputElement.value
        # H.liftEffect

setValue
    :: forall state action slots message monad
    .  MonadEffect monad
    => String
    -> H.RefLabel
    -> H.HalogenM state action slots message monad Unit
setValue value label = do
    element <- H.getRef label
    let inputElement = element >>= HTMLInputElement.fromElement
    case inputElement of
        Nothing -> pure unit
        Just inputElement' -> H.liftEffect $
            HTMLInputElement.setValue value inputElement'

getChecked :: forall state action slots message monad. MonadEffect monad =>
    H.RefLabel -> H.HalogenM state action slots message monad (Maybe Boolean)
getChecked label = do
    input <- H.getRef label
    input
        >>= HTMLInputElement.fromElement
        # traverse HTMLInputElement.checked
        # H.liftEffect

setChecked
    :: forall state action slots message monad
    .  MonadEffect monad
    => Boolean
    -> H.RefLabel
    -> H.HalogenM state action slots message monad Unit
setChecked checked label = do
    element <- H.getRef label
    let inputElement = element >>= HTMLInputElement.fromElement
    case inputElement of
        Nothing -> pure unit
        Just inputElement' -> H.liftEffect $
            HTMLInputElement.setChecked checked inputElement'

setInputValues
    :: forall monad message action state
    .  MonadEffect monad
    => ViewAccount.OkContent
    -> H.HalogenM state action ChildSlots message monad Unit
setInputValues details = do
    setValue (maybe "" identity details.discordTag) $ H.RefLabel "discord-tag"
    setValue (maybe "" identity details.birthday) $ H.RefLabel "birthday"
    void $ H.query (SProxy :: SProxy "languageInput") unit $
        MultiSelect.SetOptions
        { options: allLanguages <#> \allLanguage ->
            { option: allLanguage
            , selected: details.languages # any (_ == allLanguage)
            }
        , labeler: identity
        , comparer: (==)
        , showFilter: Just "Search languages"
        }
        unit
    void $ H.query (SProxy :: SProxy "countryInput") unit $
        SingleSelect.SetOptions
        { options: allCountries
        , selected: details.country
        , labeler: identity
        , comparer: (==)
        , showFilter: Just "Search countries"
        }
        unit
    void $ H.query (SProxy :: SProxy "timezoneInput") unit $
        SingleSelect.SetOptions
        { options: allTimezones # sortBy \leftTimezone rightTimezone -> let
            countryComparison =
                leftTimezone.country `compare` rightTimezone.country
            in
            case countryComparison of
            EQ -> leftTimezone.city `compare` rightTimezone.city
            other -> other
        , selected: details.timezone >>= \timezone' ->
            allTimezones # find (_.name >>> (_ == timezone'))
        , labeler: \{ city, country: country' } ->
            country' <> ", " <> city
        , comparer: \leftTimezone rightTimezone ->
            leftTimezone.name == rightTimezone.name
        , showFilter: Just "Search timezones"
        }
        unit
    setValue (maybe "" identity details.weekdayStart) $
        H.RefLabel "weekday-start"
    setValue (maybe "" identity details.weekdayEnd) $
        H.RefLabel "weekday-end"
    setValue (maybe "" identity details.weekendStart) $
        H.RefLabel "weekend-start"
    setValue (maybe "" identity details.weekendEnd) $
        H.RefLabel "weekend-end"
    setChecked details.hasMicrophone $ H.RefLabel "has-microphone"

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Message (Async left) Unit
handleAction Init = do
    playerInfo <- H.liftEffect getPlayerInfo
    case playerInfo of
        Nothing -> H.liftEffect $ navigate_ "/"
        Just { nickname } -> do
            details <- H.lift $ loadDetails nickname
            case details of
                Nothing -> H.put Error
                Just details' -> do
                    H.put $ Loaded
                        { nickname
                        , timezoneSet: isJust details'.timezone
                        , weekdayFrom: maybe "" identity details'.weekdayStart
                        , weekdayTo: maybe "" identity details'.weekdayEnd
                        , weekendFrom: maybe "" identity details'.weekendStart
                        , weekendTo: maybe "" identity details'.weekendEnd
                        , discordTagError: false
                        , otherError: false
                        , submitting: false
                        }
                    setInputValues details'
handleAction (TimezoneInput loadedState timezone) =
    H.put $ Loaded loadedState { timezoneSet = isJust timezone }
handleAction (WeekdayFromInput time) =
    H.modify_ case _ of
        Loaded state -> Loaded state { weekdayFrom = time }
        state -> state
handleAction (WeekdayToInput time) =
    H.modify_ case _ of
        Loaded state -> Loaded state { weekdayTo = time }
        state -> state
handleAction (WeekendFromInput time) =
    H.modify_ case _ of
        Loaded state -> Loaded state { weekendFrom = time }
        state -> state
handleAction (WeekendToInput time) =
    H.modify_ case _ of
        Loaded state -> Loaded state { weekendTo = time }
        state -> state
handleAction (Update loadedState event) = do
    H.liftEffect $ preventDefault event
    discordTag <- getValue $ H.RefLabel "discord-tag"
    birthday <- getValue $ H.RefLabel "birthday"
    languages <- H.query (SProxy :: SProxy "languageInput") unit
        (MultiSelect.GetSelected identity)
    country <- H.query (SProxy :: SProxy "countryInput") unit
        (SingleSelect.GetSelected identity)
    timezone <- H.query (SProxy :: SProxy "timezoneInput") unit
        (SingleSelect.GetSelected identity)
    weekdayStart <- getValue $ H.RefLabel "weekday-start"
    weekdayEnd <- getValue $ H.RefLabel "weekday-end"
    weekendStart <- getValue $ H.RefLabel "weekend-start"
    weekendEnd <- getValue $ H.RefLabel "weekend-end"
    hasMicrophone <- getChecked $ H.RefLabel "has-microphone"
    let resetState = loadedState
            { discordTagError = false
            , otherError      = false
            , submitting      = true
            }
    H.put $ Loaded resetState
    newState <- H.lift $ editDetails' resetState
        { discordTag:
            discordTag >>=
            case _ of
            "" -> Nothing
            discordTag' -> Just discordTag'
        , birthday:
            birthday >>=
            case _ of
            "" -> Nothing
            birthday' -> Just birthday'
        , languages    : maybe [] identity languages
        , country      : join country
        , timezone     : join timezone <#> _.name
        , weekdayStart : join timezone >>= const weekdayStart
        , weekdayEnd   : join timezone >>= const weekdayEnd
        , weekendStart : join timezone >>= const weekendStart
        , weekendEnd   : join timezone >>= const weekendEnd
        , hasMicrophone: maybe false identity hasMicrophone
        }
    case newState of
        Nothing -> H.raise $ DetailsEditted $ trim loadedState.nickname
        Just newState' -> H.put $ Loaded newState' { submitting = false }
handleAction Close = H.raise CloseClicked

component :: forall query input left.
    H.Component HH.HTML query input Message (Async left)
component = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

editDetails
    :: forall query children left
    .  (Modal.Message Message -> Maybe query)
    -> HH.ComponentHTML query (editDetails :: Slot | children) (Async left)
editDetails handleMessage = HH.slot
    (SProxy :: SProxy "editDetails") unit
    (Modal.component component) unit handleMessage
