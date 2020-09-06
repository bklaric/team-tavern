module TeamTavern.Client.Pages.Home.Wizard.EnterGeneralPlayerDetails
    (Input(..), Output(..), Slot, enterGeneralPlayerDetails) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Date as Date
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.String as String
import Data.Variant (SProxy(..))
import Effect.Now (nowDate)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.SelectDefinitive.MultiSelect (multiSelect)
import TeamTavern.Client.Components.SelectDefinitive.MultiSelect as MultiSelect
import TeamTavern.Client.Components.SelectDefinitive.SingleSelect (singleSelect)
import TeamTavern.Client.Components.SelectDefinitive.SingleSelect as SingleSelect
import TeamTavern.Client.Components.SelectDefinitive.SingleTreeSelect (singleTreeSelect)
import TeamTavern.Client.Components.SelectDefinitive.SingleTreeSelect as SingleTreeSelect
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass)
import TeamTavern.Server.Infrastructure.Languages (allLanguages)
import TeamTavern.Server.Infrastructure.Regions (Region(..), allRegions)
import TeamTavern.Server.Infrastructure.Timezones (Timezone, allTimezones)
import TeamTavern.Server.Player.ViewDetails.SendResponse (OkContent)
import Unsafe.Coerce (unsafeCoerce)

type Input = Maybe OkContent

type Output =
    { birthday :: Maybe String
    , location :: Maybe String
    , languages :: Array String
    , microphone :: Boolean
    , discordTag :: Maybe String
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    }

type Query send = { discordTagError :: Boolean, send :: send }

type State =
    { birthday :: String
    , location :: Maybe String
    , languages :: Array String
    , microphone :: Boolean
    , discordTag :: String
    , timezone :: Maybe String
    , weekdayFrom :: String
    , weekdayTo :: String
    , weekendFrom :: String
    , weekendTo :: String
    , thirteenYearsAgo :: String
    , discordTagError :: Boolean
    }

data Action
    = Initialize
    | UpdateBirthday String
    | UpdateLocation (Maybe String)
    | UpdateLanguages (Array String)
    | UpdateMicrophone Boolean
    | UpdateDiscordTag String
    | UpdateTimezone (Maybe String)
    | UpdateWeekdayFrom String
    | UpdateWeekdayTo String
    | UpdateWeekendFrom String
    | UpdateWeekendTo String

type ChildSlots =
    ( location :: SingleTreeSelect.Slot String
    , language :: MultiSelect.Slot String Unit
    , timezone :: SingleSelect.Slot Timezone Unit
    )

type Slot = H.Slot (Const Void) Output Unit

regionToOption :: Region -> SingleTreeSelect.InputEntry String
regionToOption (Region region subRegions) = SingleTreeSelect.InputEntry
    { option: region
    , subEntries: subRegions <#> regionToOption
    }

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render state =
    HH.div [ HP.class_ $ HH.ClassName "form" ]
    [ HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
        [ HH.text "Edit player details" ]
    , HH.p [ HP.class_ $ HH.ClassName "form-subheading" ]
        [ HH.text "Fill out as much as you can so other player and teams can find you more easily." ]
    , HH.h3 [ HP.class_ $ HH.ClassName "input-groups-heading" ]
        [ HH.text "Personal"]
    , HH.div [ HP.class_ $ HH.ClassName "responsive-input-groups" ]
        [ HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-calendar-alt filter-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text "Birthday" ]
                ]
            , HH.input
                [ HP.class_ $ HH.ClassName "text-line-input"
                , HP.type_ HP.InputDate
                , HP.min $ unsafeCoerce "1900-01-01"
                , HP.max $ unsafeCoerce state.thirteenYearsAgo
                , HP.value state.birthday
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
            , singleTreeSelect (SProxy :: SProxy "location")
                { entries: allRegions <#> regionToOption
                , selected: state.location
                , labeler: identity
                , comparer: (==)
                , filter: "Search locations"
                }
                \(SingleTreeSelect.SelectedChanged location) -> Just $ UpdateLocation location
            , HH.label
                [ HP.class_ $ HH.ClassName "input-underlabel" ]
                [ HH.text $ "You can select either a specific country or "
                    <> "one of the containing regions."
                ]
            ]
        ]
    , HH.h3 [ HP.class_ $ HH.ClassName "input-groups-heading" ]
        [ HH.text "Communication"]
    , HH.div [ HP.class_ $ HH.ClassName "responsive-input-groups" ]
        [ HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-comments filter-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text "Language" ]
                ]
            , multiSelect (SProxy :: SProxy "language")
                { entries: allLanguages <#> \language ->
                    { option: language
                    , selected: state.languages # Array.any (_ == language)
                    }
                , labeler: identity
                , comparer: (==)
                , filter: Just "Search languages"
                }
                \(MultiSelect.SelectedChanged languages) -> Just $ UpdateLanguages languages
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
                    [ HP.class_ $ HH.ClassName "checkbox-input"
                    , HP.type_ HP.InputCheckbox
                    , HP.checked state.microphone
                    ]
                , HH.text "I have a microphone and I'm willing to communicate."
                ]
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label"]
                [ HH.i [ HP.class_ $ HH.ClassName "fab fa-discord filter-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text "Discord tag" ]
                ]
            , HH.input
                [ HP.class_ $ HH.ClassName "text-line-input"
                , HP.type_ HP.InputText
                , HP.value state.discordTag
                ]
            , HH.label
                [ HP.class_ $ HH.ClassName "input-underlabel" ]
                [ HH.text "Example: username#1234" ]
            , HH.p
                [ HP.class_ $ inputErrorClass state.discordTagError ]
                [ HH.text $ "This does not look like a valid Discord tag." ]
            ]
        ]
    , HH.h3 [ HP.class_ $ HH.ClassName "input-groups-heading" ]
        [ HH.text "Time available"]
    , HH.div [ HP.class_ $ HH.ClassName "responsive-input-groups" ]
        [ HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-globe filter-field-icon" ] []
                , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text "Timezone" ]
                ]
            , singleSelect (SProxy :: SProxy "timezone")
                { options: allTimezones # Array.sortBy \leftTimezone rightTimezone -> let
                    countryComparison =
                        leftTimezone.country `compare` rightTimezone.country
                    in
                    case countryComparison of
                    EQ -> leftTimezone.city `compare` rightTimezone.city
                    other -> other
                , selected: state.timezone >>= \timezone ->
                    allTimezones # Array.find (_.name >>> (_ == timezone))
                , labeler: \{ city, country } ->
                    country <> ", " <> city
                , comparer: \leftTimezone rightTimezone ->
                    leftTimezone.name == rightTimezone.name
                , filter: Just "Search timezones"
                }
                \(SingleSelect.SelectedChanged timezone) -> Just $ UpdateTimezone (timezone <#> _.name)
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
                    [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                    , HP.type_ HP.InputTime
                    , HP.disabled $ isNothing state.timezone
                    , HP.value state.weekdayFrom
                    , HE.onValueChange $ Just <<< UpdateWeekdayFrom
                    ]
                , HH.span [ HP.class_ $ HH.ClassName "timespan-group-to" ] [ HH.text "to" ]
                , HH.input
                    [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                    , HP.type_ HP.InputTime
                    , HP.disabled $ isNothing state.timezone
                    , HP.value state.weekdayTo
                    , HE.onValueChange $ Just <<< UpdateWeekdayTo
                    ]
                ]
            ]
            <>
            (if isNothing state.timezone
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
                    [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                    , HP.type_ HP.InputTime
                    , HP.disabled $ isNothing state.timezone
                    , HP.value state.weekendFrom
                    , HE.onValueChange $ Just <<< UpdateWeekendFrom
                    ]
                , HH.span [ HP.class_ $ HH.ClassName "timespan-group-to" ] [ HH.text "to" ]
                , HH.input
                    [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                    , HP.type_ HP.InputTime
                    , HP.disabled $ isNothing state.timezone
                    , HP.value state.weekendTo
                    , HE.onValueChange $ Just <<< UpdateWeekendTo
                    ]
                ]
            ]
            <>
            (if isNothing state.timezone
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
        ]
    ]

formatOutput :: State -> Output
formatOutput state =
    { birthday: if state.birthday == "" then Nothing else Just state.birthday
    , location: state.location
    , languages: state.languages
    , microphone: state.microphone
    , discordTag: if state.discordTag == "" then Nothing else Just state.discordTag
    , timezone: state.timezone
    , weekdayFrom: if state.weekdayFrom == "" then Nothing else Just state.weekdayFrom
    , weekdayTo: if state.weekdayTo == "" then Nothing else Just state.weekdayTo
    , weekendFrom: if state.weekendFrom == "" then Nothing else Just state.weekendFrom
    , weekendTo: if state.weekendTo == "" then Nothing else Just state.weekendTo
    }

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    timezone <- maybe (H.liftEffect getClientTimezone) pure state.timezone
    now <- H.liftEffect nowDate
    let year = now # Date.year # fromEnum # (_ - 13)
    let month = fromEnum $ Date.month now
    let day = fromEnum $ Date.day now
    let thirteenYearsAgo
            =  show year <> "-"
            <> (if month < 10 then "0" <> show month else show month) <> "-"
            <> (if day < 10 then "0" <> show day else show day)
    H.modify_ (_
        { timezone = Just timezone
        , thirteenYearsAgo = thirteenYearsAgo
        })
    H.raise $ formatOutput state
handleAction (UpdateBirthday birthday) = do
    state <- H.modify _ { birthday = birthday }
    H.raise $ formatOutput state
handleAction (UpdateLocation location) = do
    state <- H.modify _ { location = location }
    H.raise $ formatOutput state
handleAction (UpdateLanguages languages) = do
    state <- H.modify _ { languages = languages }
    H.raise $ formatOutput state
handleAction (UpdateMicrophone microphone) = do
    state <- H.modify _ { microphone = microphone }
    H.raise $ formatOutput state
handleAction (UpdateDiscordTag discordTag) = do
    state <- H.modify _ { discordTag = discordTag }
    H.raise $ formatOutput state
handleAction (UpdateTimezone timezone) = do
    state <- H.modify _ { timezone = timezone }
    H.raise $ formatOutput state
handleAction (UpdateWeekdayFrom weekdayFrom) = do
    state <- H.modify _ { weekdayFrom = weekdayFrom }
    H.raise $ formatOutput state
handleAction (UpdateWeekdayTo weekdayTo) = do
    state <- H.modify _ { weekdayTo = weekdayTo }
    H.raise $ formatOutput state
handleAction (UpdateWeekendFrom weekendFrom) = do
    state <- H.modify _ { weekendFrom = weekendFrom }
    H.raise $ formatOutput state
handleAction (UpdateWeekendTo weekendTo) = do
    state <- H.modify _ { weekendTo = weekendTo }
    H.raise $ formatOutput state

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: case _ of
        Just input ->
            { birthday: maybe "" identity input.birthday
            , location: input.country
            , languages: input.languages
            , microphone: input.hasMicrophone
            , discordTag: maybe "" identity input.discordTag
            , timezone: input.timezone
            , weekdayFrom: maybe "" identity $ _.from <$> input.sourceWeekdayOnline
            , weekdayTo: maybe "" identity $ _.to <$> input.sourceWeekdayOnline
            , weekendFrom: maybe "" identity $ _.from <$> input.sourceWeekendOnline
            , weekendTo: maybe "" identity $ _.to <$> input.sourceWeekendOnline
            , thirteenYearsAgo: ""
            , discordTagError: false
            }
        Nothing ->
            { birthday: ""
            , location: Nothing
            , languages: []
            , microphone: false
            , discordTag: ""
            , timezone: Nothing
            , weekdayFrom: ""
            , weekdayTo: ""
            , weekendFrom: ""
            , weekendTo: ""
            , thirteenYearsAgo: ""
            , discordTagError: false
            }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

enterGeneralPlayerDetails
    :: forall action children left
    .  Input
    -> (Output -> Maybe action)
    -> HH.ComponentHTML action (enterGeneralPlayerDetails :: Slot | children) (Async left)
enterGeneralPlayerDetails input handleMessage =
    HH.slot (SProxy :: SProxy "enterGeneralPlayerDetails") unit component input handleMessage
