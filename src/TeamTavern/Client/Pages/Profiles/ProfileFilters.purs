module TeamTavern.Client.Pages.Profiles.ProfileFilters where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.SelectDefinitive.MultiSelect (multiSelect, multiSelectIndexed)
import TeamTavern.Client.Components.SelectDefinitive.MultiSelect as MultiSelect
import TeamTavern.Client.Components.SelectDefinitive.MultiTreeSelect (multiTreeSelect)
import TeamTavern.Client.Components.SelectDefinitive.MultiTreeSelect as MultiTreeSelect
import TeamTavern.Server.Infrastructure.Languages (allLanguages)
import TeamTavern.Server.Infrastructure.Regions (Region(..), allRegions)
import Web.HTML as Html
import Web.HTML.Window as Window

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

type Filters =
    { ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , countries :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , fields :: Array
        { fieldKey :: String
        , optionKey :: String
        }
    , newOrReturning :: Boolean
    }

type Input = { fields :: Array Field, filters :: Filters }

type State =
    { ageFrom :: String
    , ageTo :: String
    , countries :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , weekdayFrom :: String
    , weekdayTo :: String
    , weekendFrom :: String
    , weekendTo :: String
    , fields :: Array
        { selected :: Array Option
        , field :: Field
        }
    , newOrReturning :: Boolean
    , filtersVisible :: Boolean
    , playerFiltersVisible :: Boolean
    , gameFiltersVisible :: Boolean
    }

data Action
    = Initialize
    | Receive Input
    | ApplyAction
    | Clear
    | AgeFromInput String
    | AgeToInput String
    | LanguagesMessage (MultiSelect.Output String)
    | CountriesInput (MultiTreeSelect.Output String)
    | MicrophoneInput Boolean
    | WeekdayFromInput String
    | WeekdayToInput String
    | WeekendFromInput String
    | WeekendToInput String
    | FieldInput String (MultiSelect.Output Option)
    | NewOrReturningInput Boolean
    | ToggleFiltersVisibility
    | TogglePlayerFiltersVisibility
    | ToggleGameFiltersVisibility

data Output = Apply Filters

type Slot = H.Slot (Const Void) Output Unit

type ChildSlots =
    ( language :: MultiSelect.Slot String Unit
    , country :: MultiTreeSelect.Slot String
    , field :: MultiSelect.Slot Option String
    )

regionToOption :: Region -> MultiTreeSelect.InputEntry String
regionToOption (Region region subRegions) = MultiTreeSelect.InputEntry
    { option: region
    , subEntries: subRegions <#> regionToOption
    }

fieldLabel :: forall slots action. String -> String -> HH.HTML slots action
fieldLabel label icon = HH.label
    [ HP.class_ $ HH.ClassName "input-label", HP.for label ]
    [ HH.i [ HP.class_ $ HH.ClassName $ icon <> " filter-field-icon" ] []
    , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text label ]
    ]

fieldInput
    :: forall left
    .  { field :: Field, selected :: Array Option }
    -> H.ComponentHTML Action ChildSlots (Async left)
fieldInput { field: { key, label, icon, options }, selected } =
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel label icon
    , multiSelectIndexed (SProxy :: SProxy "field") key
        { options
        , selected
        , labeler: _.label
        , comparer: \leftOption rightOption ->
            leftOption.key == rightOption.key
        , filter: Nothing
        }
        (Just <<< FieldInput key)
    ]

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render state = HH.div [ HP.class_ $ HH.ClassName "filters-card" ] $
    [ HH.h3
        [ HP.class_ $ HH.ClassName "filters-title"
        , HE.onClick $ const $ Just ToggleFiltersVisibility
        ]
        [ HH.text "Profile filters"
        , HH.i
            [ HP.class_ $ HH.ClassName $ "fas filters-title-caret "
                <> if state.filtersVisible then "fa-caret-up" else "fa-caret-down"
            ]
            []
        ]
    ]
    <>
    if state.filtersVisible
    then
        [ HH.h4
            [ HP.class_ $ HH.ClassName "card-section-title"
            , HE.onClick $ const $ Just TogglePlayerFiltersVisibility
            ]
            [ HH.text "Player filters"
            , HH.i
                [ HP.class_ $ HH.ClassName $ "fas filters-section-title-caret "
                    <> if state.playerFiltersVisible then "fa-caret-up" else "fa-caret-down"
                ]
                []
            ]
        ]
        <>
        (if state.playerFiltersVisible
        then Array.singleton $
            HH.div [ HP.class_ $ HH.ClassName "card-section" ]
            [ HH.div [ HP.class_ $ HH.ClassName "filter-input-groups" ] $
                [ HH.div [ HP.class_ $ HH.ClassName "input-group" ]
                    [ fieldLabel "Age" "fas fa-calendar-alt"
                    , HH.div [ HP.class_ $ HH.ClassName "timespan-group" ]
                        [ HH.span [ HP.class_ $ HH.ClassName "timespan-group-from" ] [ HH.text "From" ]
                        , HH.input
                            [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                            , HP.type_ HP.InputNumber
                            , HP.value state.ageFrom
                            , HE.onValueChange $ Just <<< AgeFromInput
                            ]
                        , HH.span [ HP.class_ $ HH.ClassName "timespan-group-to" ] [ HH.text "to" ]
                        , HH.input
                            [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                            , HP.type_ HP.InputNumber
                            , HP.value state.ageTo
                            , HE.onValueChange $ Just <<< AgeToInput
                            ]
                        ]
                    ]
                , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
                    [ fieldLabel "Location" "fas fa-globe-europe"
                    , multiTreeSelect (SProxy :: SProxy "country")
                        { entries: allRegions <#> regionToOption
                        , selected: state.countries
                        , labeler: identity
                        , comparer: (==)
                        , filter: "Search locations"
                        }
                        (Just <<< CountriesInput)
                    ]
                , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
                    [ fieldLabel "Language" "fas fa-comments"
                    , multiSelect (SProxy :: SProxy "language")
                        { options: allLanguages
                        , selected: state.languages
                        , labeler: identity
                        , comparer: (==)
                        , filter: Just "Search languages"
                        }
                        (Just <<< LanguagesMessage)
                    ]
                , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
                    [ fieldLabel "Microphone" "fas fa-microphone"
                    , HH.label
                        [ HP.class_ $ HH.ClassName "checkbox-input-label" ]
                        [ HH.input
                            [ HP.class_ $ HH.ClassName "checkbox-input"
                            , HP.type_ HP.InputCheckbox
                            , HP.checked state.microphone
                            , HE.onChecked $ Just <<< MicrophoneInput
                            ]
                        , HH.text "Must have a microphone and be willing to communicate."
                        ]
                    ]
                , HH.div [ HP.class_ $ HH.ClassName "input-group" ] $
                    [ fieldLabel "Online on weekdays" "fas fa-clock"
                    , HH.div [ HP.class_ $ HH.ClassName "timespan-group" ]
                        [ HH.span [ HP.class_ $ HH.ClassName "timespan-group-from" ] [ HH.text "From" ]
                        , HH.input
                            [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                            , HP.type_ HP.InputTime
                            , HP.value state.weekdayFrom
                            , HE.onValueChange $ Just <<< WeekdayFromInput
                            ]
                        , HH.span [ HP.class_ $ HH.ClassName "timespan-group-to" ] [ HH.text "to" ]
                        , HH.input
                            [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                            , HP.type_ HP.InputTime
                            , HP.value state.weekdayTo
                            , HE.onValueChange $ Just <<< WeekdayToInput
                            ]
                        ]
                    ]
                    <>
                    if String.null state.weekdayFrom && (not $ String.null state.weekdayTo)
                        || (not $ String.null state.weekdayFrom) && String.null state.weekdayTo
                    then Array.singleton $
                        HH.label
                        [ HP.class_ $ HH.ClassName "input-underlabel" ]
                        [ HH.text $ "Enter both times for the filter to have effect." ]
                    else []
                , HH.div [ HP.class_ $ HH.ClassName "input-group" ] $
                    [ fieldLabel "Online on weekends" "fas fa-clock"
                    , HH.div [ HP.class_ $ HH.ClassName "timespan-group" ]
                        [ HH.span [ HP.class_ $ HH.ClassName "timespan-group-from" ] [ HH.text "From" ]
                        , HH.input
                            [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                            , HP.type_ HP.InputTime
                            , HP.value state.weekendFrom
                            , HE.onValueChange $ Just <<< WeekendFromInput
                            ]
                        , HH.span [ HP.class_ $ HH.ClassName "timespan-group-to" ] [ HH.text "to" ]
                        , HH.input
                            [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                            , HP.type_ HP.InputTime
                            , HP.value state.weekendTo
                            , HE.onValueChange $ Just <<< WeekendToInput
                            ]
                        ]
                    ]
                    <>
                    if String.null state.weekendFrom && (not $ String.null state.weekendTo)
                        || (not $ String.null state.weekendFrom) && String.null state.weekendTo
                    then Array.singleton $
                        HH.label
                        [ HP.class_ $ HH.ClassName "input-underlabel" ]
                        [ HH.text $ "Enter both times for the filter to have effect." ]
                    else []
                ]
            ]
        else [])
        <>
        [ HH.h4
            [ HP.class_ $ HH.ClassName "card-section-title"
            , HE.onClick $ const $ Just ToggleGameFiltersVisibility
            ]
            [ HH.text "Game filters"
            , HH.i
                [ HP.class_ $ HH.ClassName $ "fas filters-section-title-caret "
                    <> if state.gameFiltersVisible then "fa-caret-up" else "fa-caret-down"
                ]
                []
            ]
        ]
        <>
        (if state.gameFiltersVisible
        then Array.singleton $
            HH.div [ HP.class_ $ HH.ClassName "card-section" ]
            [ HH.div [ HP.class_ $ HH.ClassName "filter-input-groups" ] $
                (map fieldInput state.fields)
                <>
                [ HH.div [ HP.class_ $ HH.ClassName "input-group" ]
                    [ HH.label
                        [ HP.class_ $ HH.ClassName "input-label" ] $
                        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-book filter-field-icon" ] []
                        , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text "New or returning player" ]
                        ]
                    , HH.label
                        [ HP.class_ $ HH.ClassName "checkbox-input-label" ]
                        [ HH.input
                            [ HP.class_ $ HH.ClassName "checkbox-input"
                            , HP.type_ HP.InputCheckbox
                            , HP.checked state.newOrReturning
                            , HE.onChecked (Just <<< NewOrReturningInput)
                            ]
                        , HH.text "Must be new or returning players to the game."
                        ]
                    ]
                ]
            ]
        else [])
        <>
        [ HH.div [ HP.class_ $ HH.ClassName "card-section" ]
            [ HH.button
                [ HP.class_ $ HH.ClassName "apply-filters"
                , HE.onClick $ const $ Just $ ApplyAction
                ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-filter button-icon" ] []
                , HH.text "Apply filters"
                ]
            , HH.button
                [ HP.class_ $ HH.ClassName "clear-filters"
                , HE.onClick $ const $ Just $ Clear
                ]
                [ HH.i [ HP.class_ $ HH.ClassName "fas fa-eraser button-icon" ] []
                , HH.text "Clear filters"
                ]
            ]
        ]
    else []

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction Initialize = do
    windowWidth <- Html.window >>= Window.innerWidth # H.liftEffect
    let showFilters = windowWidth >= 960
    H.modify_ _
        { filtersVisible = showFilters
        , playerFiltersVisible = showFilters
        , gameFiltersVisible = showFilters
        }
handleAction (Receive { fields, filters }) = do
    H.modify_ \state -> state
        { ageFrom = maybe "" show filters.ageFrom
        , ageTo = maybe "" show filters.ageTo
        , countries = filters.countries
        , languages = filters.languages
        , microphone = filters.microphone
        , weekdayFrom = maybe "" identity filters.weekdayFrom
        , weekdayTo = maybe "" identity filters.weekdayTo
        , weekendFrom = maybe "" identity filters.weekendFrom
        , weekendTo = maybe "" identity filters.weekendTo
        , fields = fields <#> \field ->
            { field
            , selected:
                field.options # Array.filter \option ->
                    filters.fields # Array.any \{ fieldKey, optionKey } ->
                        fieldKey == field.key && optionKey == option.key
            }
        , newOrReturning = filters.newOrReturning
        }
handleAction ApplyAction = do
    state <- H.get
    let nothingIfNull string = if String.null string then Nothing else Just string
    let ageFrom = Int.fromString state.ageFrom
        ageTo = Int.fromString state.ageTo
        countries = state.countries
        languages = state.languages
        microphone = state.microphone
        weekdayFrom = nothingIfNull state.weekdayFrom
        weekdayTo = nothingIfNull state.weekdayTo
        weekendFrom = nothingIfNull state.weekendFrom
        weekendTo = nothingIfNull state.weekendTo
        fields = state.fields
            <#> (\{ field, selected } ->
                selected <#> \option -> { optionKey: option.key, fieldKey: field.key })
            # join
        newOrReturning = state.newOrReturning
    H.raise $ Apply
        { ageFrom, ageTo, languages, countries, microphone
        , weekdayFrom, weekdayTo, weekendFrom, weekendTo
        , fields, newOrReturning
        }
handleAction Clear = do
    H.modify_ \state -> state
        { ageFrom = ""
        , ageTo = ""
        , countries = []
        , languages = []
        , microphone = false
        , weekdayFrom = ""
        , weekdayTo = ""
        , weekendFrom = ""
        , weekendTo = ""
        , fields = state.fields <#> \field -> field { selected = [] }
        , newOrReturning = false
        }
handleAction (AgeFromInput ageFrom) =
    H.modify_ (_ { ageFrom = ageFrom })
handleAction (AgeToInput ageTo) =
    H.modify_ (_ { ageTo = ageTo })
handleAction (CountriesInput (MultiTreeSelect.SelectedChanged countries)) =
    H.modify_ (_ { countries = countries })
handleAction (LanguagesMessage (MultiSelect.SelectedChanged languages)) =
    H.modify_ _ { languages = languages }
handleAction (MicrophoneInput microphone) =
    H.modify_ (_ { microphone = microphone })
handleAction (WeekdayFromInput time) =
    H.modify_ (_ { weekdayFrom = time })
handleAction (WeekdayToInput time) =
    H.modify_ (_ { weekdayTo = time })
handleAction (WeekendFromInput time) =
    H.modify_ (_ { weekendFrom = time })
handleAction (WeekendToInput time) =
    H.modify_ (_ { weekendTo = time })
handleAction (FieldInput fieldKey (MultiSelect.SelectedChanged options)) =
    H.modify_ \state -> state
        { fields = state.fields <#> \stateField ->
            if stateField.field.key == fieldKey
            then stateField { selected = options }
            else stateField
        }
handleAction (NewOrReturningInput newOrReturning) =
    H.modify_ (_ { newOrReturning = newOrReturning })
handleAction ToggleFiltersVisibility =
    H.modify_ \state -> state { filtersVisible = not state.filtersVisible }
handleAction TogglePlayerFiltersVisibility =
    H.modify_ \state -> state { playerFiltersVisible = not state.playerFiltersVisible }
handleAction ToggleGameFiltersVisibility =
    H.modify_ \state -> state { gameFiltersVisible = not state.gameFiltersVisible }

initialState :: Input -> State
initialState { fields, filters } =
    { ageFrom: maybe "" show filters.ageFrom
    , ageTo: maybe "" show filters.ageTo
    , countries: filters.countries
    , languages: filters.languages
    , microphone: filters.microphone
    , weekdayFrom: maybe "" identity filters.weekdayFrom
    , weekdayTo: maybe "" identity filters.weekdayTo
    , weekendFrom: maybe "" identity filters.weekendFrom
    , weekendTo: maybe "" identity filters.weekendTo
    , fields: fields <#> \field ->
        { field
        , selected:
            field.options # Array.filter \option ->
                filters.fields # Array.any \{ fieldKey, optionKey } ->
                    fieldKey == field.key && optionKey == option.key
        }
    , newOrReturning: filters.newOrReturning
    , filtersVisible: false
    , playerFiltersVisible: false
    , gameFiltersVisible: false
    }

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

profileFilters
    :: forall action children left
    .  Input
    -> (Output -> Maybe action)
    -> HH.ComponentHTML action (profileFilters :: Slot | children) (Async left)
profileFilters input handleOutput =
    HH.slot (SProxy :: SProxy "profileFilters") unit component input handleOutput
