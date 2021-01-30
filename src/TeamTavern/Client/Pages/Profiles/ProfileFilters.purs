module TeamTavern.Client.Pages.Profiles.ProfileFilters (Option, Field, Filters, Input, Output(..), Slot, profileFilters) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.MultiMap (MultiMap)
import Data.MultiMap as MultiMap
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import TeamTavern.Client.Components.Card (cardHeading, cardSection, cardSectionHeading)
import TeamTavern.Client.Components.Input (externalIdCheckboxes, inputGroup, inputLabel)
import TeamTavern.Client.Components.InputGroup (timeRangeInputGroup)
import TeamTavern.Client.Components.Select.MultiSelect as MultiSelect
import TeamTavern.Client.Components.Select.MultiTreeSelect as MultiTreeSelect
import TeamTavern.Client.Components.Team.ProfileInputGroup (FieldValues, fieldInputGroup, newOrReturningInputGroup)
import TeamTavern.Client.Components.Team.TeamInputGroup (ageInputGroup, languagesInputGroup, locationInputGroup, microphoneInputGroup)
import TeamTavern.Client.Pages.Profiles.GameHeader (Tab(..))
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.ExternalIdIlk (ExternalIdIlk, ExternalIdIlks)
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
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , externalIdIlks :: Array ExternalIdIlk
    , fieldValues :: FieldValues
    , newOrReturning :: Boolean
    }

type Input =
    { externalIdIlks :: ExternalIdIlks
    , fields :: Array Field
    , filters :: Filters
    , tab :: Tab
    }

type State =
    { ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , allExternalIdIlks :: ExternalIdIlks
    , selectedExternalIdIlks :: Array ExternalIdIlk
    , fields :: Array Field
    , fieldValues :: FieldValues
    , newOrReturning :: Boolean
    , filtersVisible :: Boolean
    , playerFiltersVisible :: Boolean
    , profileFiltersVisible :: Boolean
    , tab :: Tab
    }

data Action
    = Initialize
    | Receive Input
    | ApplyFilters
    | ClearFilters
    | UpdateAgeFrom (Maybe Int)
    | UpdateAgeTo (Maybe Int)
    | UpdateLanguages (MultiSelect.Output String)
    | UpdateLocations (MultiTreeSelect.Output String)
    | UpdateMicrophone Boolean
    | UpdateWeekdayFrom (Maybe String)
    | UpdateWeekdayTo (Maybe String)
    | UpdateWeekendFrom (Maybe String)
    | UpdateWeekendTo (Maybe String)
    | UpdateExternalIdIlk ExternalIdIlk
    | UpdateFieldValues String (MultiSelect.Output Option)
    | UpdateNewOrReturning Boolean
    | ToggleFiltersVisibility
    | TogglePlayerFiltersVisibility
    | ToggleProfileFiltersVisibility

data Output = Apply Filters

type Slot = H.Slot (Const Void) Output Unit

type ChildSlots =
    ( language :: MultiSelect.Slot String Unit
    , location :: MultiTreeSelect.Slot String
    , multiSelectField :: MultiSelect.Slot Option String
    )

headerCaret :: forall action slots. String -> Boolean -> HH.HTML slots action
headerCaret class_ visible =
    HH.i [ HS.class_ $ "fas " <> class_ <> if visible then " fa-caret-up" else " fa-caret-down" ] []

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render state =
    HH.div [ HS.class_ "filters-card" ] $
    [ HH.div
        [ HS.class_ "card-header"
        , HE.onClick $ const $ Just ToggleFiltersVisibility
        ]
        [ cardHeading "Filters"
        , headerCaret "filters-title-caret" state.filtersVisible
        ]
    ]
    <>
    if state.filtersVisible
    then
        [ HH.div
            [ HS.class_ "card-section-header"
            , HE.onClick $ const $ Just TogglePlayerFiltersVisibility
            ]
            [ cardSectionHeading
                case state.tab of
                Players -> "Player details"
                Teams -> "Team details"
            , headerCaret "filters-section-title-caret" state.playerFiltersVisible
            ]
        ]
        <>
        (if state.playerFiltersVisible
        then Array.singleton $
            cardSection
            [ HH.div [ HS.class_ "filter-input-groups" ] $
                [ ageInputGroup state.ageFrom state.ageTo UpdateAgeFrom UpdateAgeTo
                , locationInputGroup state.locations UpdateLocations
                , languagesInputGroup state.languages UpdateLanguages
                , microphoneInputGroup state.microphone UpdateMicrophone
                , timeRangeInputGroup "Online on weekdays" false state.weekdayFrom state.weekdayTo
                    UpdateWeekdayFrom UpdateWeekdayTo
                , timeRangeInputGroup "Online on weekends" false state.weekendFrom state.weekendTo
                    UpdateWeekendFrom UpdateWeekendTo
                ]
            ]
        else [])
        <>
        [ HH.div
            [ HS.class_ "card-section-header"
            , HE.onClick $ const $ Just ToggleProfileFiltersVisibility
            ]
            [ cardSectionHeading "Profile details"
            , headerCaret "filters-section-title-caret" state.profileFiltersVisible
            ]
        ]
        <>
        (if state.profileFiltersVisible
        then Array.singleton $
            cardSection
            [ HH.div [ HS.class_ "filter-input-groups" ] $
                ( case externalIdCheckboxes state.allExternalIdIlks state.selectedExternalIdIlks UpdateExternalIdIlk of
                    Nothing -> []
                    Just checkboxes -> [ inputGroup [ inputLabel "fas fa-laptop" "Platform", checkboxes ] ]
                )
                <> ( state.fields <#> fieldInputGroup state.fieldValues UpdateFieldValues )
                <> [ newOrReturningInputGroup state.newOrReturning UpdateNewOrReturning ]
            ]
        else [])
        <>
        [ cardSection
            [ HH.button
                [ HS.class_ "apply-filters"
                , HE.onClick $ const $ Just $ ApplyFilters
                ]
                [ HH.i [ HS.class_ "fas fa-filter button-icon" ] []
                , HH.text "Apply filters"
                ]
            , HH.button
                [ HS.class_ "clear-filters"
                , HE.onClick $ const $ Just $ ClearFilters
                ]
                [ HH.i [ HS.class_ "fas fa-eraser button-icon" ] []
                , HH.text "Clear filters"
                ]
            ]
        ]
    else []

handleAction :: forall left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction Initialize = do
    windowWidth <- Html.window >>= Window.innerWidth # H.liftEffect
    let showFilters = windowWidth >= 960
    H.modify_ _
        { filtersVisible = showFilters
        , playerFiltersVisible = showFilters
        , profileFiltersVisible = showFilters
        }
handleAction (Receive { externalIdIlks, fields, filters, tab }) = do
    H.modify_ _
        { ageFrom = filters.ageFrom
        , ageTo = filters.ageTo
        , locations = filters.locations
        , languages = filters.languages
        , microphone = filters.microphone
        , weekdayFrom = filters.weekdayFrom
        , weekdayTo = filters.weekdayTo
        , weekendFrom = filters.weekendFrom
        , weekendTo = filters.weekendTo
        , allExternalIdIlks = externalIdIlks
        , selectedExternalIdIlks = filters.externalIdIlks
        , fields = fields
        , fieldValues = filters.fieldValues
        , newOrReturning = filters.newOrReturning
        , tab = tab
        }
handleAction ApplyFilters = do
    state <- H.get
    H.raise $ Apply
        { ageFrom: state.ageFrom
        , ageTo: state.ageTo
        , languages: state.languages
        , locations: state.locations
        , microphone: state.microphone
        , weekdayFrom: state.weekdayFrom
        , weekdayTo: state.weekdayTo
        , weekendFrom: state.weekendFrom
        , weekendTo: state.weekendTo
        , externalIdIlks: state.selectedExternalIdIlks
        , fieldValues: state.fieldValues
        , newOrReturning: state.newOrReturning
        }
handleAction ClearFilters = do
    H.modify_ \state -> state
        { ageFrom = Nothing
        , ageTo = Nothing
        , locations = []
        , languages = []
        , microphone = false
        , weekdayFrom = Nothing
        , weekdayTo = Nothing
        , weekendFrom = Nothing
        , weekendTo = Nothing
        , selectedExternalIdIlks = []
        , fieldValues = (MultiMap.empty :: MultiMap String String)
        , newOrReturning = false
        }
handleAction (UpdateAgeFrom ageFrom) =
    H.modify_ (_ { ageFrom = ageFrom })
handleAction (UpdateAgeTo ageTo) =
    H.modify_ (_ { ageTo = ageTo })
handleAction (UpdateLocations locations) =
    H.modify_ (_ { locations = locations })
handleAction (UpdateLanguages languages) =
    H.modify_ _ { languages = languages }
handleAction (UpdateMicrophone microphone) =
    H.modify_ (_ { microphone = microphone })
handleAction (UpdateWeekdayFrom time) =
    H.modify_ (_ { weekdayFrom = time })
handleAction (UpdateWeekdayTo time) =
    H.modify_ (_ { weekdayTo = time })
handleAction (UpdateWeekendFrom time) =
    H.modify_ (_ { weekendFrom = time })
handleAction (UpdateWeekendTo time) =
    H.modify_ (_ { weekendTo = time })
handleAction (UpdateExternalIdIlk externalIdIlk) =
    H.modify_ \state -> state
        { selectedExternalIdIlks =
            if Array.elem externalIdIlk state.selectedExternalIdIlks
            then Array.delete externalIdIlk state.selectedExternalIdIlks
            else Array.cons externalIdIlk state.selectedExternalIdIlks
        }
handleAction (UpdateFieldValues fieldKey options) = do
    H.modify_ \state -> state
        { fieldValues =
            case NonEmptyList.fromFoldable options of
            Nothing -> MultiMap.delete fieldKey state.fieldValues
            Just options' -> MultiMap.insertOrReplace fieldKey (_.key <$> options') state.fieldValues
        }
handleAction (UpdateNewOrReturning newOrReturning) =
    H.modify_ (_ { newOrReturning = newOrReturning })
handleAction ToggleFiltersVisibility =
    H.modify_ \state -> state { filtersVisible = not state.filtersVisible }
handleAction TogglePlayerFiltersVisibility =
    H.modify_ \state -> state { playerFiltersVisible = not state.playerFiltersVisible }
handleAction ToggleProfileFiltersVisibility =
    H.modify_ \state -> state { profileFiltersVisible = not state.profileFiltersVisible }

initialState :: Input -> State
initialState { externalIdIlks, fields, filters, tab } =
    { ageFrom: filters.ageFrom
    , ageTo: filters.ageTo
    , locations: filters.locations
    , languages: filters.languages
    , microphone: filters.microphone
    , weekdayFrom: filters.weekdayFrom
    , weekdayTo: filters.weekdayTo
    , weekendFrom: filters.weekendFrom
    , weekendTo: filters.weekendTo
    , allExternalIdIlks: externalIdIlks
    , selectedExternalIdIlks: []
    , fields
    , fieldValues: filters.fieldValues
    , newOrReturning: filters.newOrReturning
    , filtersVisible: false
    , playerFiltersVisible: false
    , profileFiltersVisible: false
    , tab
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
