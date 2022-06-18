module TeamTavern.Client.Pages.Profiles.ProfileFilters (Option, Field, Input, Output(..), Slot, profileFilters) where

import Prelude

import Async (Async)
import CSS as Css
import Data.Array as Array
import Data.Const (Const)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.MultiMap (MultiMap)
import Data.MultiMap as MultiMap
import Type.Proxy (Proxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Record as Record
import Record.Extra (pick)
import TeamTavern.Client.Components.Ads (filtersMpu)
import TeamTavern.Client.Components.Button (button)
import TeamTavern.Client.Components.Card (card, cardHeading, cardSection, cardSectionHeading)
import TeamTavern.Client.Components.Input (inputGroup, inputLabel)
import TeamTavern.Client.Components.InputGroup (timeRangeInputGroup)
import TeamTavern.Client.Components.Select.MultiSelect as MultiSelect
import TeamTavern.Client.Components.Select.MultiTreeSelect as MultiTreeSelect
import TeamTavern.Client.Components.Team.ProfileInputGroup (FieldValues, fieldInputGroup, newOrReturningInputGroup)
import TeamTavern.Client.Components.Team.TeamInputGroup (ageInputGroup, languagesInputGroup, locationInputGroup, microphoneInputGroup)
import TeamTavern.Client.Pages.Profile.Filters (Filters)
import TeamTavern.Client.Pages.Profiles.CreateAlert (createAlert)
import TeamTavern.Client.Pages.Profiles.CreateAlert as CreateAlert
import TeamTavern.Client.Pages.Profiles.GameHeader (ProfileTab(..))
import TeamTavern.Client.Pages.Profiles.TeamBadge (organizationCheckboxBadges, platformCheckboxBadges, sizeCheckboxBadges)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.CreateAlert as CreateAlertRoute
import TeamTavern.Routes.Shared.Organization (Organization)
import TeamTavern.Routes.Shared.Platform (Platform, Platforms)
import TeamTavern.Routes.Shared.Size (Size)
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

type Input =
    { platforms :: Platforms
    , fields :: Array Field
    , filters :: Filters
    , tab :: ProfileTab
    , handle :: String
    }

type State =
    { organizations :: Array Organization
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , sizes :: Array Size
    , allPlatforms :: Platforms
    , selectedPlatforms :: Array Platform
    , fields :: Array Field
    , fieldValues :: FieldValues
    , newOrReturning :: Boolean
    , filtersVisible :: Boolean
    , playerFiltersVisible :: Boolean
    , profileFiltersVisible :: Boolean
    , tab :: ProfileTab
    , handle :: String
    , createAlertModalShown :: Boolean
    }

data Action
    = Initialize
    | Receive Input
    | ApplyFilters
    | ClearFilters
    | UpdateOrganization Organization
    | UpdateAgeFrom (Maybe Int)
    | UpdateAgeTo (Maybe Int)
    | UpdateLanguages (MultiSelect.Output String)
    | UpdateLocations (MultiTreeSelect.Output String)
    | UpdateMicrophone Boolean
    | UpdateWeekdayFrom (Maybe String)
    | UpdateWeekdayTo (Maybe String)
    | UpdateWeekendFrom (Maybe String)
    | UpdateWeekendTo (Maybe String)
    | UpdateSize Size
    | UpdatePlatform Platform
    | UpdateFieldValues String (MultiSelect.Output Option)
    | UpdateNewOrReturning Boolean
    | ToggleFiltersVisibility
    | TogglePlayerFiltersVisibility
    | ToggleProfileFiltersVisibility
    | ShowCreateAlertModal
    | HideCreateAlertModal

data Output = Apply Filters

type Slot = H.Slot (Const Void) Output Unit

type ChildSlots =
    ( language :: MultiSelect.Slot String Unit
    , location :: MultiTreeSelect.Slot String
    , multiSelectField :: MultiSelect.Slot Option String
    , createAlert :: CreateAlert.Slot
    )

headerCaret :: forall action slots. String -> Boolean -> HH.HTML slots action
headerCaret class_ visible =
    HH.i [ HS.class_ $ "fas " <> class_ <> if visible then " fa-caret-up" else " fa-caret-down" ] []

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render state =
    HH.div_ $ [
    card $
    [ HH.div
        [ HS.class_ "card-header"
        , HE.onClick $ const ToggleFiltersVisibility
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
            , HE.onClick $ const TogglePlayerFiltersVisibility
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
                ( case state.tab of
                    Players -> []
                    Teams -> Array.singleton $
                        inputGroup
                        [ inputLabel "fas fa-users" "Organization"
                        , HH.div [ HC.style $ Css.height $ Css.px 7.0 ] [] -- filler
                        , organizationCheckboxBadges state.organizations UpdateOrganization
                        ]
                )
                <>
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
            , HE.onClick $ const ToggleProfileFiltersVisibility
            ]
            [ cardSectionHeading "Game details"
            , headerCaret "filters-section-title-caret" state.profileFiltersVisible
            ]
        ]
        <>
        (if state.profileFiltersVisible
        then Array.singleton $
            cardSection
            [ HH.div [ HS.class_ "filter-input-groups" ] $
                ( case state.tab of
                    Players -> []
                    Teams -> Array.singleton $
                        inputGroup
                        [ inputLabel "fas fa-users" "Size"
                        , HH.div [ HC.style $ Css.height $ Css.px 7.0 ] [] -- filler
                        , sizeCheckboxBadges state.sizes UpdateSize
                        ]
                )
                <> guard (not $ Array.null state.allPlatforms.tail)
                [ inputGroup
                    [ inputLabel "fas fa-laptop" "Platform"
                    , HH.div [ HC.style $ Css.height $ Css.px 7.0 ] [] -- filler
                    , platformCheckboxBadges state.allPlatforms state.selectedPlatforms UpdatePlatform
                    ]
                ]
                <> ( state.fields <#> fieldInputGroup state.fieldValues UpdateFieldValues )
                <> [ newOrReturningInputGroup state.newOrReturning UpdateNewOrReturning ]
            ]
        else [])
        <>
        [ HH.div [ HS.class_ "filters-buttons" ]
            [ button "filters-clear-button" "fas fa-eraser" "Clear filters" ClearFilters
            , button "filters-apply-button" "fas fa-filter" "Apply filters" ApplyFilters
            , button "filters-alert-button" "fas fa-bell"
                ( case state.tab of
                    Players -> "Create player profile alert"
                    Teams -> "Create team profile alert"
                )
                ShowCreateAlertModal
            ]
        ]
    else []
    ]
    <> [ filtersMpu ]
    <> guard state.createAlertModalShown
        [ createAlert
            { handle: state.handle
            , playerOrTeam:
                case state.tab of
                Players -> CreateAlertRoute.Player
                Teams -> CreateAlertRoute.Team
            , filters: state # Record.insert (Proxy :: _ "platforms") state.selectedPlatforms # pick
            }
            (const HideCreateAlertModal)
        ]

handleAction :: forall left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction Initialize = do
    windowWidth <- Html.window >>= Window.innerWidth # H.liftEffect
    let showFilters = windowWidth >= 960
    H.modify_ _
        { filtersVisible = showFilters
        , playerFiltersVisible = showFilters
        , profileFiltersVisible = showFilters
        }
handleAction (Receive { platforms, fields, filters, tab }) = do
    H.modify_ _
        { organizations = filters.organizations
        , ageFrom = filters.ageFrom
        , ageTo = filters.ageTo
        , locations = filters.locations
        , languages = filters.languages
        , microphone = filters.microphone
        , weekdayFrom = filters.weekdayFrom
        , weekdayTo = filters.weekdayTo
        , weekendFrom = filters.weekendFrom
        , weekendTo = filters.weekendTo
        , sizes = filters.sizes
        , allPlatforms = platforms
        , selectedPlatforms = filters.platforms
        , fields = fields
        , fieldValues = filters.fieldValues
        , newOrReturning = filters.newOrReturning
        , tab = tab
        }
handleAction ApplyFilters = do
    state <- H.get
    H.raise $ Apply
        { organizations: state.organizations
        , ageFrom: state.ageFrom
        , ageTo: state.ageTo
        , languages: state.languages
        , locations: state.locations
        , microphone: state.microphone
        , weekdayFrom: state.weekdayFrom
        , weekdayTo: state.weekdayTo
        , weekendFrom: state.weekendFrom
        , weekendTo: state.weekendTo
        , sizes: state.sizes
        , platforms: state.selectedPlatforms
        , fieldValues: state.fieldValues
        , newOrReturning: state.newOrReturning
        }
handleAction ClearFilters = do
    H.modify_ \state -> state
        { organizations = []
        , ageFrom = Nothing
        , ageTo = Nothing
        , locations = []
        , languages = []
        , microphone = false
        , weekdayFrom = Nothing
        , weekdayTo = Nothing
        , weekendFrom = Nothing
        , weekendTo = Nothing
        , sizes = []
        , selectedPlatforms = []
        , fieldValues = (MultiMap.empty :: MultiMap String String)
        , newOrReturning = false
        }
handleAction ShowCreateAlertModal =
    H.modify_ _ { createAlertModalShown = true }
handleAction HideCreateAlertModal =
    H.modify_ _ { createAlertModalShown = false }
handleAction (UpdateOrganization teamOrganization) =
    H.modify_ \state -> state
        { organizations =
            if Array.elem teamOrganization state.organizations
            then Array.delete teamOrganization state.organizations
            else Array.cons teamOrganization state.organizations
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
handleAction (UpdateSize teamSize) =
    H.modify_ \state -> state
        { sizes =
            if Array.elem teamSize state.sizes
            then Array.delete teamSize state.sizes
            else Array.cons teamSize state.sizes
        }
handleAction (UpdatePlatform platform) =
    H.modify_ \state -> state
        { selectedPlatforms =
            if Array.elem platform state.selectedPlatforms
            then Array.delete platform state.selectedPlatforms
            else Array.cons platform state.selectedPlatforms
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
initialState { platforms, fields, filters, tab, handle } =
    { organizations: filters.organizations
    , ageFrom: filters.ageFrom
    , ageTo: filters.ageTo
    , locations: filters.locations
    , languages: filters.languages
    , microphone: filters.microphone
    , weekdayFrom: filters.weekdayFrom
    , weekdayTo: filters.weekdayTo
    , weekendFrom: filters.weekendFrom
    , weekendTo: filters.weekendTo
    , sizes: filters.sizes
    , allPlatforms: platforms
    , selectedPlatforms: filters.platforms
    , fields
    , fieldValues: filters.fieldValues
    , newOrReturning: filters.newOrReturning
    , filtersVisible: false
    , playerFiltersVisible: false
    , profileFiltersVisible: false
    , tab
    , handle
    , createAlertModalShown: false
    }

component :: forall query left. H.Component query Input Output (Async left)
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
    -> (Output -> action)
    -> HH.ComponentHTML action (profileFilters :: Slot | children) (Async left)
profileFilters input handleOutput =
    HH.slot (Proxy :: _ "profileFilters") unit component input handleOutput
