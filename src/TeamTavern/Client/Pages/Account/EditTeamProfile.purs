module TeamTavern.Client.Pages.Account.EditTeamProfile where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (any, foldl)
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.Foldable (find, intercalate)
import Data.HTTP.Method (Method(..))
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Options ((:=))
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..), match)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.CloseButton (closeButton)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.MultiSelect (multiSelectIndexed)
import TeamTavern.Client.Components.MultiSelect as MultiSelect
import TeamTavern.Client.Components.SelectDeclarative.MultiSelect2 as MultiSelect2
import TeamTavern.Client.Components.SelectDeclarative.SingleSelect2 as SingleSelect2
import TeamTavern.Client.Components.SelectDeclarative.TreeSelect (treeSelect)
import TeamTavern.Client.Components.SelectDeclarative.TreeSelect as TreeSelect
import TeamTavern.Client.Components.SingleSelect as SingleSelect
import TeamTavern.Client.Pages.Account.Types (Nickname)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Infrastructure.Languages (allLanguages)
import TeamTavern.Server.Infrastructure.Regions (Region(..), allRegions)
import TeamTavern.Server.Infrastructure.Timezones (Timezone, allTimezones)
import TeamTavern.Server.Profile.UpdatePlayerProfile.SendResponse as Update
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

type Option =
    { key :: String
    , label :: String
    }

type Field =
    { key :: String
    , label :: String
    , icon :: String
    , options :: Array Option
    }

type FieldValue =
    { fieldKey :: String
    , optionKeys :: Array String
    }

type Input =
    { nickname :: Nickname
    , handle :: String
    , title :: String
    , age :: { from :: Maybe Int, to :: Maybe Int }
    , countries :: Array String
    , languages :: Array String
    , hasMicrophone :: Boolean
    , timezone :: Maybe String
    , weekdayOnline :: Maybe
        { clientFrom :: String
        , clientTo :: String
        , sourceFrom :: String
        , sourceTo :: String
        }
    , weekendOnline :: Maybe
        { clientFrom :: String
        , clientTo :: String
        , sourceFrom :: String
        , sourceTo :: String
        }
    , fields :: Array Field
    , fieldValues :: Array FieldValue
    , summary :: Array String
    }

type State =
    { nickname :: Nickname
    , handle :: String
    , title :: String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , languages :: Array String
    , countries :: Array String
    , hasMicrophone :: Boolean
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , fields :: Array Field
    , fieldValues :: Array FieldValue
    , summary :: String
    , summaryError :: Boolean
    , otherError :: Boolean
    , submitting :: Boolean
    }

data Action
    = SummaryInput String
    | AgeFromInput String
    | AgeToInput String
    | LanguageInput (MultiSelect2.Output String)
    | RegionInput (TreeSelect.Output String)
    | MicrophoneInput Boolean
    | TimezoneInput (SingleSelect2.Output Timezone)
    | WeekdayFromInput String
    | WeekdayToInput String
    | WeekendFromInput String
    | WeekendToInput String
    | Update Event
    | Close

data Output = ProfileUpdated Nickname | CloseClicked

type ChildSlots =
    ( "language" :: MultiSelect2.Slot String Unit
    , "country" :: TreeSelect.Slot String
    , "timezone" :: SingleSelect2.Slot Timezone Unit
    , "singleSelectField" :: SingleSelect.Slot Option String
    , "multiSelectField" :: MultiSelect.Slot Option String
    )

type Slot = H.Slot (Modal.Query Input (Const Void)) (Modal.Message Output)

regionToEntry :: Region -> TreeSelect.InputEntry String
regionToEntry (Region region subRegions) = TreeSelect.InputEntry
    { option: region
    , subEntries: subRegions <#> regionToEntry
    }

fieldLabel :: forall slots action.
    String -> String -> HH.HTML slots action
fieldLabel label icon =
    HH.label
    [ HP.class_ $ HH.ClassName "input-label" ]
    [ HH.i [ HP.class_ $ HH.ClassName $ icon <> " filter-field-icon" ] []
    , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text label ]
    , divider
    , HH.span [ HP.class_ $ H.ClassName "input-sublabel" ] [ HH.text "optional" ]
    ]

fieldInput
    :: forall left
    .  Array FieldValue
    -> Field
    -> H.ComponentHTML Action ChildSlots (Async left)
fieldInput fieldValues { key, label, icon, options } = let
    fieldValue' = fieldValues # find \{ fieldKey } -> fieldKey == key
    selectedOptionIds' = fieldValue' <#> _.optionKeys
    in
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel label icon
    , multiSelectIndexed (SProxy :: SProxy "multiSelectField") key
        { options: options <#> \option ->
            { option
            , selected: selectedOptionIds' # maybe false \selectedOptionIds ->
                selectedOptionIds # any (_ == option.key) }
        , labeler: _.label
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        , showFilter: Nothing
        }
    ]

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render state @
    { title
    , fields
    , summary
    , summaryError
    , fieldValues
    , otherError
    , submitting
    } =
    HH.div [ HP.class_ $ HH.ClassName "wide-single-form-container" ] $ pure $ HH.form
    [ HP.class_ $ H.ClassName "form", HE.onSubmit $ Just <<< Update ] $
    [ closeButton Close
    , HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
        [ HH.text $ "Edit your " <> title <> " profile" ]
    , HH.p [ HP.class_ $ HH.ClassName "form-subheading" ]
        [ HH.text "Describe yourself as a player and let other players find you." ]
    , HH.div [ HP.class_ $ HH.ClassName "responsive-input-groups" ] $
        [ HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ fieldLabel "Age" "fas fa-calendar-alt"
            , HH.div [ HP.class_ $ HH.ClassName "timespan-group" ]
                [ HH.span [ HP.class_ $ HH.ClassName "timespan-group-from" ] [ HH.text "From" ]
                , HH.input
                    [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                    , HP.type_ HP.InputNumber
                    , HP.value $ maybe "" show state.ageFrom
                    , HE.onValueChange $ Just <<< AgeFromInput
                    ]
                , HH.span [ HP.class_ $ HH.ClassName "timespan-group-to" ] [ HH.text "to" ]
                , HH.input
                    [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                    , HP.type_ HP.InputNumber
                    , HP.value $ maybe "" show state.ageTo
                    , HE.onValueChange $ Just <<< AgeToInput
                    ]
                ]
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ fieldLabel "Language" "fas fa-comments"
            , MultiSelect2.multiSelect (SProxy :: SProxy "language")
                { entries: allLanguages <#> \language ->
                    { option: language
                    , selected: state.languages # Array.any (_ == language)
                    }
                , labeler: identity
                , comparer: (==)
                , filter: Just "Search languages"
                }
                (Just <<< LanguageInput)
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ fieldLabel "Country" "fas fa-globe-europe"
            , treeSelect (SProxy :: SProxy "country")
                { entries: allRegions <#> regionToEntry
                , selected: state.countries
                , labeler: identity
                , comparer: (==)
                , filter: "Search countries"
                }
                (Just <<< RegionInput)
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ fieldLabel "Microphone" "fas fa-microphone"
            , HH.label
                [ HP.class_ $ HH.ClassName "checkbox-input-label" ]
                [ HH.input
                    [ HP.class_ $ HH.ClassName "checkbox-input"
                    , HP.type_ HP.InputCheckbox
                    , HP.checked state.hasMicrophone
                    , HE.onChecked $ Just <<< MicrophoneInput
                    ]
                , HH.text "Must have a microphone and be willing to communicate."
                ]
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ fieldLabel "Timezone" "fas fa-globe"
            , SingleSelect2.singleSelect (SProxy :: SProxy "timezone")
                { options: allTimezones # Array.sortBy \leftTimezone rightTimezone -> let
                    countryComparison =
                        leftTimezone.country `compare` rightTimezone.country
                    in
                    case countryComparison of
                    EQ -> leftTimezone.city `compare` rightTimezone.city
                    other -> other
                , selected: state.timezone >>= \timezone ->
                    allTimezones # Array.find (_.name >>> (_ == timezone))
                , labeler: \{ city, country: country' } ->
                    country' <> ", " <> city
                , comparer: \leftTimezone rightTimezone ->
                    leftTimezone.name == rightTimezone.name
                , filter: Just "Search timezones"
                }
                (Just <<< TimezoneInput)
            ]
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ] $
            [ fieldLabel "Online on weekdays" "fas fa-clock"
            , HH.div [ HP.class_ $ HH.ClassName "timespan-group" ]
                [ HH.span [ HP.class_ $ HH.ClassName "timespan-group-from" ] [ HH.text "From" ]
                , HH.input
                    [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                    , HP.type_ HP.InputTime
                    , HP.disabled $ isNothing state.timezone
                    , HP.value $ maybe "" identity state.weekdayFrom
                    , HE.onValueChange $ Just <<< WeekdayFromInput
                    ]
                , HH.span [ HP.class_ $ HH.ClassName "timespan-group-to" ] [ HH.text "to" ]
                , HH.input
                    [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                    , HP.type_ HP.InputTime
                    , HP.disabled $ isNothing state.timezone
                    , HP.value $ maybe "" identity state.weekdayTo
                    , HE.onValueChange $ Just <<< WeekdayToInput
                    ]
                ]
            ]
            <>
            if isNothing state.timezone
            then Array.singleton $
                HH.label
                    [ HP.class_ $ HH.ClassName "input-underlabel" ]
                    [ HH.text $ "Set your timezone to unlock this field." ]
            else if isNothing state.weekdayFrom && isJust state.weekdayTo
                || isJust state.weekdayFrom && isNothing state.weekdayTo
            then Array.singleton $
                HH.label
                [ HP.class_ $ HH.ClassName "input-underlabel" ]
                [ HH.text $ "Enter both times for the field to have effect." ]
            else []
        , HH.div [ HP.class_ $ HH.ClassName "input-group" ] $
            [ fieldLabel "Online on weekends" "fas fa-clock"
            , HH.div [ HP.class_ $ HH.ClassName "timespan-group" ]
                [ HH.span [ HP.class_ $ HH.ClassName "timespan-group-from" ] [ HH.text "From" ]
                , HH.input
                    [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                    , HP.type_ HP.InputTime
                    , HP.disabled $ isNothing state.timezone
                    , HP.value $ maybe "" identity state.weekendFrom
                    , HE.onValueChange $ Just <<< WeekendFromInput
                    ]
                , HH.span [ HP.class_ $ HH.ClassName "timespan-group-to" ] [ HH.text "to" ]
                , HH.input
                    [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                    , HP.type_ HP.InputTime
                    , HP.disabled $ isNothing state.timezone
                    , HP.value $ maybe "" identity state.weekendTo
                    , HE.onValueChange $ Just <<< WeekendToInput
                    ]
                ]
            ]
            <>
            if isNothing state.timezone
            then Array.singleton $
                HH.label
                    [ HP.class_ $ HH.ClassName "input-underlabel" ]
                    [ HH.text $ "Set your timezone to unlock this field." ]
            else if isNothing state.weekendFrom && isJust state.weekendTo
                || isJust state.weekendFrom && isNothing state.weekendTo
            then Array.singleton $
                HH.label
                [ HP.class_ $ HH.ClassName "input-underlabel" ]
                [ HH.text $ "Enter both times for the field to have effect." ]
            else []
        ]
        <>
        (fields <#> fieldInput fieldValues)
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label"
            , HP.for "summary"
            ]
            [ HH.text "Summary" ]
        , HH.textarea
            [ HP.id_ "summary"
            , HP.class_ $ HH.ClassName "text-input"
            , HE.onValueInput $ Just <<< SummaryInput
            , HP.value summary
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass summaryError ]
            [ HH.text
                "The summary cannot be empty and cannot be more than 2000 characters long." ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "form-submit-button"
        , HP.disabled $ summary == "" || submitting
        ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user-edit button-icon" ] []
        , HH.text
            if submitting
            then "Editing profile..."
            else "Edit profile"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

data UpdateError
    = Other
    | Content
        { summary :: Boolean
        , url :: Array String
        , missing :: Array String
        }

updateProfile :: forall left.
    State -> Async left (Maybe UpdateError)
updateProfile state @ { handle, nickname } = Async.unify do
    response <-
        Fetch.fetch
        ("/api/profiles/by-handle/" <> handle <> "/teams/" <> nickname)
        (  Fetch.method := PUT
        <> Fetch.body := Json.writeJSON
            { summary: state.summary
            , ageFrom: state.ageFrom
            , ageTo: state.ageTo
            , countries: state.countries
            , languages: state.languages
            , hasMicrophone: state.hasMicrophone
            , timezone: state.timezone
            , weekdayFrom: state.weekdayFrom
            , weekdayTo: state.weekdayTo
            , weekendFrom: state.weekendFrom
            , weekendTo: state.weekendTo
            , fieldValues: state.fieldValues # Array.filter
                \{ optionKeys } -> not $ Array.null optionKeys
            }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just Other)
    result <- case FetchRes.status response of
        204 -> pure Nothing
        400 ->  FetchRes.text response
            >>= JsonAsync.readJSON
            #   bimap
                (const $ Just Other)
                (\(content :: Update.BadRequestContent) -> Just $ Content $
                    match
                    { invalidProfile:
                        foldl
                        (\errors error ->
                            match
                            { invalidSummary: const $ errors { summary = true }
                            , invalidUrl: \{ fieldKey } ->
                                errors { url = Array.cons fieldKey errors.url }
                            , missing: \{ fieldKey } ->
                                errors { missing = Array.cons fieldKey errors.missing }
                            }
                            error
                        )
                        { summary: false, url: [], missing: [] }
                    }
                    content
                )
        _ -> pure $ Just Other
    pure result

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (SummaryInput summary) =
    H.modify_ (_ { summary = summary })
handleAction (AgeFromInput ageFrom) =
    H.modify_ (_ { ageFrom = Int.fromString ageFrom })
handleAction (AgeToInput ageTo) =
    H.modify_ (_ { ageTo = Int.fromString ageTo })
handleAction (LanguageInput (MultiSelect2.SelectedChanged languages)) =
    H.modify_ (_ { languages = languages })
handleAction (RegionInput (TreeSelect.SelectedChanged countries)) =
    H.modify_ (_ { countries = countries })
handleAction (MicrophoneInput hasMicrophone) =
    H.modify_ (_ { hasMicrophone = hasMicrophone })
handleAction (TimezoneInput (SingleSelect2.SelectedChanged timezone)) =
    H.modify_ (_ { timezone = timezone <#> _.name })
handleAction (WeekdayFromInput time) =
    H.modify_ (_ { weekdayFrom = if String.null time then Nothing else Just time })
handleAction (WeekdayToInput time) =
    H.modify_ (_ { weekdayTo = if String.null time then Nothing else Just time })
handleAction (WeekendFromInput time) =
    H.modify_ (_ { weekendFrom = if String.null time then Nothing else Just time })
handleAction (WeekendToInput time) =
    H.modify_ (_ { weekendTo = if String.null time then Nothing else Just time })
handleAction (Update event) = do
    H.liftEffect $ preventDefault event
    state @ { fields } <- H.modify (_
        { summaryError   = false
        , otherError     = false
        , submitting     = true
        })
    multiSelectResults <-
        (H.queryAll (SProxy :: SProxy "multiSelectField")
        $ MultiSelect.Selected identity)
    let (multiSelectValues :: Array _) =
            multiSelectResults
            # Map.toUnfoldable
            <#> \(Tuple fieldKey options) ->
                { fieldKey
                , optionKeys: options <#> _.key
                }
    let state' = state { fieldValues = multiSelectValues }
    result <- H.lift $ updateProfile state'
    case result of
        Nothing -> H.raise $ ProfileUpdated state.nickname
        Just Other -> H.put $ state' { otherError = true, submitting = false }
        Just (Content errors) -> H.put $ state'
            { summaryError = errors.summary
            , submitting = false
            }
handleAction Close = H.raise CloseClicked

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: \
        { handle
        , title
        , nickname
        , age
        , countries
        , languages
        , hasMicrophone
        , timezone
        , weekdayOnline
        , weekendOnline
        , summary
        , fieldValues
        , fields
        } ->
        { handle
        , title
        , nickname
        , ageFrom: age.from
        , ageTo: age.to
        , countries
        , languages
        , hasMicrophone
        , timezone
        , weekdayFrom: weekdayOnline <#> _.sourceFrom
        , weekdayTo: weekdayOnline <#> _.sourceTo
        , weekendFrom: weekendOnline <#> _.sourceFrom
        , weekendTo: weekendOnline <#> _.sourceTo
        , fields
        , summary: intercalate "\n\n" summary
        , summaryError: false
        , fieldValues: fields <#> (\field ->
            case fieldValues # find \{ fieldKey } -> fieldKey == field.key of
            Nothing ->
                { fieldKey: field.key
                , optionKeys: []
                }
            Just { optionKeys } ->
                { fieldKey: field.key
                , optionKeys
                })
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

editTeamProfile
    :: forall query children left
    .  (Modal.Message Output -> Maybe query)
    -> HH.ComponentHTML query (editProfile :: Slot Unit | children) (Async left)
editTeamProfile handleMessage = HH.slot
    (SProxy :: SProxy "editProfile") unit
    (Modal.component component) unit handleMessage
