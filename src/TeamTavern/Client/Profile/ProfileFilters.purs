module TeamTavern.Client.Profile.ProfileFilters where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Symbol (SProxy(..))
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Select.TreeSelect (treeSelect)
import TeamTavern.Client.Components.Select.TreeSelect as TreeSelect
import TeamTavern.Client.Components.SelectDeclarative.MultiSelect (multiSelect, multiSelectIndexed)
import TeamTavern.Client.Components.SelectDeclarative.MultiSelect as MultiSelect
import TeamTavern.Server.Infrastructure.Languages (allLanguages)
import TeamTavern.Server.Infrastructure.Regions (Region(..), allRegions)
import Unsafe.Coerce (unsafeCoerce)

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
    , languages :: Array String
    -- , countries :: Array String
    , microphone :: Boolean
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , fields :: Array
        { fieldKey :: String
        , optionKey :: String
        }
    }

type Input = Array Field

type State =
    { ageFrom :: String
    , ageTo :: String
    , languages :: MultiSelect.Input String
    , countries :: TreeSelect.Input String
    , microphone :: Boolean
    , weekdayFrom :: String
    , weekdayTo :: String
    , weekendFrom :: String
    , weekendTo :: String
    , fields :: Array
        { input :: MultiSelect.Input Option
        , field :: Field
        }
    }

data Action
    = ApplyAction
    | Clear
    | AgeFromInput String
    | AgeToInput String
    | LanguagesMessage (MultiSelect.Output String)
    -- | CountriesInput (TreeSelect.Message String)
    | MicrophoneInput Boolean
    | WeekdayFromInput String
    | WeekdayToInput String
    | WeekendFromInput String
    | WeekendToInput String
    | FieldInput String (Array (MultiSelect.InputEntry Option))

data Output = Apply Filters

type Slot = H.Slot (Const Void) Output Unit

type ChildSlots =
    ( language :: MultiSelect.Slot String Unit
    , country :: TreeSelect.Slot String
    , field :: MultiSelect.Slot Option String
    )

emptyFilters :: Filters
emptyFilters =
    { ageFrom: Nothing
    , ageTo: Nothing
    , languages: []
    , microphone: false
    , weekdayFrom: Nothing
    , weekdayTo: Nothing
    , weekendFrom: Nothing
    , weekendTo: Nothing
    , fields: []
    }

fieldLabel :: forall slots action. String -> String -> HH.HTML slots action
fieldLabel label icon = HH.label
    [ HP.class_ $ HH.ClassName "input-label", HP.for label ]
    [ HH.i [ HP.class_ $ HH.ClassName $ icon <> " filter-field-icon" ] []
    , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text label ]
    ]

fieldInput
    :: forall left
    .  { input :: MultiSelect.Input Option, field :: Field }
    -> H.ComponentHTML Action ChildSlots (Async left)
fieldInput { input, field: { key, label, icon } } =
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel label icon
    , multiSelectIndexed (SProxy :: SProxy "field") key input
        case _ of
        MultiSelect.SelectedChanged entries -> Just $ FieldInput key entries
        MultiSelect.FilterChanged _ -> Nothing
    ]

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render state = HH.div [ HP.class_ $ HH.ClassName "card" ]
    [ HH.span [ HP.class_ $ HH.ClassName "card-title" ]
        [ HH.text "Profile filters" ]
    , HH.div [ HP.class_ $ HH.ClassName "card-content" ]
        [ HH.div [ HP.class_ $ HH.ClassName "responsive-input-groups" ] $
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
                [ fieldLabel "Language" "fas fa-comments"
                , multiSelect (SProxy :: SProxy "language") state.languages (Just <<< LanguagesMessage)
                ]
            , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
                [ fieldLabel "Country" "fas fa-globe-europe"
                , treeSelect (SProxy :: SProxy "country")
                    { options: allRegions <#> regionToOption
                    , labeler: identity
                    , comparer: (==)
                    , placeholder: "Search countries"
                    }
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
                    [ HH.text $ "Enter both times for the filter to have effect."
                    ]
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
                    [ HH.text $ "Enter both times for the filter to have effect."
                    ]
                else []
            ]
            <>
            (map fieldInput state.fields)
        , HH.button
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

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction ApplyAction = do
    state <- H.get
    let nothingIfNull string = if String.null string then Nothing else Just string
    let ageFrom = Int.fromString state.ageFrom
        ageTo = Int.fromString state.ageTo
        languages =
            state.languages.entries
            # Array.filter _.selected
            <#> _.option
        -- countries = state.countries.options <#> ...
        microphone = state.microphone
        weekdayFrom = nothingIfNull state.weekdayFrom
        weekdayTo = nothingIfNull state.weekdayTo
        weekendFrom = nothingIfNull state.weekendFrom
        weekendTo = nothingIfNull state.weekendTo
        fields =
            state.fields
            <#> (\{ input, field } ->
                input.entries
                # Array.filter _.selected
                <#> \{ option } ->
                    { optionKey: option.key, fieldKey: field.key })
            # join
    H.raise $ Apply
        { ageFrom, ageTo, languages, microphone
        , weekdayFrom, weekdayTo, weekendFrom, weekendTo, fields
        }
handleAction Clear = do
    H.modify_ \state -> state
        { ageFrom = ""
        , ageTo = ""
        , languages = state.languages
            { entries = state.languages.entries <#> (_ { selected = false }) }
        , microphone = false
        , weekdayFrom = ""
        , weekdayTo = ""
        , weekendFrom = ""
        , weekendTo = ""
        , fields = state.fields <#> \stateField -> stateField
            { input = stateField.input
                { entries = stateField.input.entries
                    <#> (_ { selected = false })
                }
            }
        }
    state <- H.get
    log $ unsafeCoerce state
handleAction (AgeFromInput ageFrom) =
    H.modify_ (_ { ageFrom = ageFrom })
handleAction (AgeToInput ageTo) =
    H.modify_ (_ { ageTo = ageTo })
handleAction (LanguagesMessage (MultiSelect.SelectedChanged entries)) =
    H.modify_ \state -> state
        { languages = state.languages { entries = entries } }
handleAction (LanguagesMessage (MultiSelect.FilterChanged text)) =
    H.modify_ \state -> state
        { languages = state.languages
            { filter = state.languages.filter <#> (_ { text = text }) }
        }
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
handleAction (FieldInput fieldKey entries) =
    H.modify_ \state -> state
        { fields = state.fields <#> \stateField ->
            if stateField.field.key == fieldKey
            then stateField { input = stateField.input { entries = entries } }
            else stateField
        }

regionToOption :: Region -> TreeSelect.Option String
regionToOption (Region region subRegions) = TreeSelect.Option
    { option: region
    , subOptions: subRegions <#> regionToOption
    }

initialState :: Array Field -> State
initialState fields =
    { ageFrom: ""
    , ageTo: ""
    , languages:
        { entries: allLanguages <#> { option: _, selected: false }
        , labeler: identity
        , comparer: (==)
        , filter: Just { placeholder: "Search languages", text: "" }
        }
    , countries:
        { options: allRegions <#> regionToOption
        , labeler: identity
        , comparer: (==)
        , placeholder: "Search countries"
        }
    , microphone: false
    , weekdayFrom: ""
    , weekdayTo: ""
    , weekendFrom: ""
    , weekendTo: ""
    , fields: fields <#> \field ->
        { input:
            { entries: field.options <#> { option: _, selected: false }
            , labeler: _.label
            , comparer: \leftOption rightOption ->
                leftOption.key == rightOption.key
            , filter: Nothing
            }
        , field
        }
    }

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

filterProfiles
    :: forall query children left
    .  (Array Field)
    -> (Output -> Maybe query)
    -> HH.ComponentHTML query (filterProfiles :: Slot | children) (Async left)
filterProfiles fields handleOutput =
    HH.slot (SProxy :: SProxy "filterProfiles") unit component fields handleOutput
