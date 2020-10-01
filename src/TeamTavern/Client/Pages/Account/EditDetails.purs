module TeamTavern.Client.Pages.Account.EditDetails
    (Input(..), Output(..), Slot, editDetails) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (sortBy)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.Date as Date
import Data.Enum (fromEnum)
import Data.Foldable (any, find, foldl)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Options ((:=))
import Data.String as String
import Data.Traversable (traverse)
import Data.Variant (SProxy(..), match)
import Effect.Class (class MonadEffect)
import Effect.Now (nowDate)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.CloseButton (closeButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.SelectDeclarative.SingleTreeSelect (singleTreeSelect)
import TeamTavern.Client.Components.SelectDeclarative.SingleTreeSelect as SingleTreeSelect
import TeamTavern.Client.Components.SelectImperative.MultiSelect (multiSelect)
import TeamTavern.Client.Components.SelectImperative.MultiSelect as MultiSelect
import TeamTavern.Client.Components.SelectImperative.SingleSelect (singleSelect')
import TeamTavern.Client.Components.SelectImperative.SingleSelect as SingleSelect
import TeamTavern.Client.Pages.Account.Types (Nickname)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Infrastructure.Languages (allLanguages)
import TeamTavern.Server.Infrastructure.Regions (Region(..), allRegions)
import TeamTavern.Server.Infrastructure.Timezones (Timezone, allTimezones)
import TeamTavern.Server.Player.UpdateDetails.SendResponse as Update
import TeamTavern.Server.Player.ViewDetails.SendResponse as ViewAccount
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLInputElement as HTMLInputElement

data Input = Input Nickname ViewAccount.OkContent

data Action
    = Initialize
    | TimezoneInput (Maybe Timezone)
    | WeekdayFromInput String
    | WeekdayToInput String
    | WeekendFromInput String
    | WeekendToInput String
    | CountryInput (SingleTreeSelect.Output String)
    | Update Event
    | Close

data Output = DetailsEditted String | CloseClicked

type State =
    { nickname :: String
    , country :: Maybe String
    , timezoneSet :: Boolean
    , weekdayFrom :: String
    , weekdayTo :: String
    , weekendFrom :: String
    , weekendTo :: String
    , discordTagError :: Boolean
    , otherError :: Boolean
    , submitting :: Boolean
    , details :: ViewAccount.OkContent
    , thirteenYearsAgo :: String
    }

type ChildSlots =
    ( languageInput :: MultiSelect.Slot String
    , countryInput :: SingleTreeSelect.Slot String
    , timezoneInput :: SingleSelect.Slot Timezone
    )

type Slot = H.Slot (Modal.Query Input (Const Void)) (Modal.Message Output) Unit

regionToOption :: Region -> SingleTreeSelect.InputEntry String
regionToOption (Region region subRegions) = SingleTreeSelect.InputEntry
    { option: region
    , subEntries: subRegions <#> regionToOption
    }

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render state @ { timezoneSet, discordTagError, otherError, submitting, thirteenYearsAgo } =
    HH.div [ HP.class_ $ HH.ClassName "wide-single-form-container" ] $ pure $
    HH.form
    [ HP.class_ $ H.ClassName "form", HE.onSubmit $ Just <<< Update ]
    [ closeButton Close
    , HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
        [ HH.text "Edit player details" ]
    , HH.div [ HP.class_ $ HH.ClassName "responsive-input-groups" ]
        [ HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-calendar-alt filter-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text "Birthday" ]
                ]
            , HH.input
                [ HP.ref $ H.RefLabel "birthday"
                , HP.class_ $ HH.ClassName "text-line-input"
                , HP.type_ HP.InputDate
                , HP.min $ unsafeCoerce "1900-01-01"
                , HP.max $ unsafeCoerce thirteenYearsAgo
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
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-globe-europe filter-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text "Location" ]
                ]
            , singleTreeSelect (SProxy :: SProxy "countryInput")
                { entries: allRegions <#> regionToOption
                , selected: state.country
                , labeler: identity
                , comparer: (==)
                , filter: "Search locations"
                }
                (Just <<< CountryInput)
            , HH.label
                [ HP.class_ $ HH.ClassName "input-underlabel" ]
                [ HH.text $ "You can select either a specific country or "
                    <> "one of the containing regions."
                ]
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-comments filter-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text "Language" ]
                ]
            , multiSelect (SProxy :: SProxy "languageInput")
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-microphone filter-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text "Microphone" ]
                ]
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
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-globe filter-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text "Timezone" ]
                ]
            , singleSelect' (SProxy :: SProxy "timezoneInput")
                (\(SingleSelect.SelectedChanged option) ->
                    Just $ TimezoneInput option)
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ] $
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-clock filter-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text "Online on weekdays" ]
                ]
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
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-clock filter-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text "Online on weekends" ]
                ]
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
                [ HP.class_ $ HH.ClassName "input-label"]
                [ HH.i [ HP.class_ $ HH.ClassName "fab fa-discord filter-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text "Discord tag" ]
                ]
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

type EditDetailsBody =
    { discordTag :: Maybe String
    , birthday :: Maybe String
    , languages :: Array String
    , country :: Maybe String
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , hasMicrophone :: Boolean
    }

editDetails' :: forall left.
    State -> EditDetailsBody -> Async left (Maybe State)
editDetails' state details = Async.unify do
    response <-
        Fetch.fetch
        ("/api/players/by-nickname/" <> state.nickname <> "/details")
        (  Fetch.method := PUT
        <> Fetch.body := Json.writeJSON details
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
                        , invalidAbout:
                            const $ state'
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
    => Maybe Timezone
    -> ViewAccount.OkContent
    -> H.HalogenM state action ChildSlots message monad Unit
setInputValues selectedTimezone details = do
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
    void $ H.query (SProxy :: SProxy "timezoneInput") unit $
        SingleSelect.SetOptions
        { options: allTimezones # sortBy \leftTimezone rightTimezone -> let
            countryComparison =
                leftTimezone.country `compare` rightTimezone.country
            in
            case countryComparison of
            EQ -> leftTimezone.city `compare` rightTimezone.city
            other -> other
        , selected: selectedTimezone
        , labeler: \{ city, country: country' } ->
            country' <> ", " <> city
        , comparer: \leftTimezone rightTimezone ->
            leftTimezone.name == rightTimezone.name
        , showFilter: Just "Search timezones"
        }
        unit
    setValue (maybe "" identity $ _.from <$> details.sourceWeekdayOnline) $
        H.RefLabel "weekday-start"
    setValue (maybe "" identity $ _.to <$> details.sourceWeekdayOnline) $
        H.RefLabel "weekday-end"
    setValue (maybe "" identity $ _.from <$> details.sourceWeekendOnline) $
        H.RefLabel "weekend-start"
    setValue (maybe "" identity $ _.to <$> details.sourceWeekendOnline) $
        H.RefLabel "weekend-end"
    setChecked details.hasMicrophone $ H.RefLabel "has-microphone"

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    selectedTimezoneName <-
        case state.details.timezone of
        Nothing -> H.liftEffect getClientTimezone
        Just timezone' -> pure timezone'
    let selectedTimezone = allTimezones
            # find (_.name >>> (_ == selectedTimezoneName))
    setInputValues selectedTimezone state.details
    now <- H.liftEffect nowDate
    let year = now # Date.year # fromEnum # (_ - 13)
    let month = fromEnum $ Date.month now
    let day = fromEnum $ Date.day now
    let thirteenYearsAgo
            =  show year <> "-"
            <> (if month < 10 then "0" <> show month else show month) <> "-"
            <> (if day < 10 then "0" <> show day else show day)
    H.modify_ (_
        { timezoneSet = isJust selectedTimezone
        , thirteenYearsAgo = thirteenYearsAgo
        })
handleAction (TimezoneInput timezone) =
    H.modify_ (_ { timezoneSet = isJust timezone })
handleAction (WeekdayFromInput time) =
    H.modify_ (_ { weekdayFrom = time })
handleAction (WeekdayToInput time) =
    H.modify_ (_ { weekdayTo = time })
handleAction (WeekendFromInput time) =
    H.modify_ (_ { weekendFrom = time })
handleAction (WeekendToInput time) =
    H.modify_ (_ { weekendTo = time })
handleAction (CountryInput (SingleTreeSelect.SelectedChanged option)) =
    H.modify_ (_ { country = option })
handleAction (Update event) = do
    H.liftEffect $ preventDefault event
    state <- H.get
    discordTag <- getValue $ H.RefLabel "discord-tag"
    birthday <- getValue $ H.RefLabel "birthday"
    languages <- H.query (SProxy :: SProxy "languageInput") unit
        (MultiSelect.GetSelected identity)
    timezone <- H.query (SProxy :: SProxy "timezoneInput") unit
        (SingleSelect.GetSelected identity)
    weekdayFrom <- getValue $ H.RefLabel "weekday-start"
    weekdayTo <- getValue $ H.RefLabel "weekday-end"
    weekendFrom <- getValue $ H.RefLabel "weekend-start"
    weekendTo <- getValue $ H.RefLabel "weekend-end"
    hasMicrophone <- getChecked $ H.RefLabel "has-microphone"
    let resetState = state
            { discordTagError = false
            , otherError      = false
            , submitting      = true
            }
    H.put resetState
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
        , languages   : maybe [] identity languages
        , country     : state.country
        , timezone    : join timezone <#> _.name
        , weekdayFrom : join timezone >>= const weekdayFrom
        , weekdayTo   : join timezone >>= const weekdayTo
        , weekendFrom : join timezone >>= const weekendFrom
        , weekendTo   : join timezone >>= const weekendTo
        , hasMicrophone: maybe false identity hasMicrophone
        }
    case newState of
        Nothing -> H.raise $ DetailsEditted $ state.nickname
        Just newState' -> H.put $ newState' { submitting = false }
handleAction Close = H.raise CloseClicked

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: \(Input nickname details) ->
        { nickname
        , details
        , country: details.country
        , timezoneSet: isJust details.timezone
        , weekdayFrom: maybe "" identity $ _.from <$> details.sourceWeekdayOnline
        , weekdayTo: maybe "" identity $ _.to <$> details.sourceWeekdayOnline
        , weekendFrom: maybe "" identity $ _.from <$> details.sourceWeekendOnline
        , weekendTo: maybe "" identity $ _.to <$> details.sourceWeekendOnline
        , discordTagError: false
        , otherError: false
        , submitting: false
        , thirteenYearsAgo: ""
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

editDetails
    :: forall action children left
    .  (Modal.Message Output -> Maybe action)
    -> HH.ComponentHTML action (editDetails :: Slot | children) (Async left)
editDetails handleMessage = HH.slot
    (SProxy :: SProxy "editDetails") unit
    (Modal.component component) unit handleMessage
