module TeamTavern.Client.Components.Team.TeamFormInput (Input, Output, Slot, emptyInput, teamFormInput) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Variant (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Record.Extra (pick)
import TeamTavern.Client.Components.Input (inputGroupsHeading, responsiveInputGroups)
import TeamTavern.Client.Components.InputGroup (timeRangeInputGroup, timezoneInputGroup)
import TeamTavern.Client.Components.Select.MultiSelect as MultiSelect
import TeamTavern.Client.Components.Select.MultiTreeSelect as MultiTreeSelect
import TeamTavern.Client.Components.Select.SingleSelect as SingleSelect
import TeamTavern.Client.Components.Team.OrganizationInfo (organizationInfo)
import TeamTavern.Client.Components.Team.OrganizationInfo as OrganizationInfo
import TeamTavern.Client.Components.Team.TeamInputGroup (ageInputGroup, languagesInputGroup, locationInputGroup, microphoneInputGroup, nameInputGroup, websiteInputGroup)
import TeamTavern.Client.Pages.Profiles.TeamBadge (organizationRadioBadges)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.Organization (Organization, OrganizationNW(..), fromOrganizationNW, toOrganizationNW)
import TeamTavern.Server.Infrastructure.Timezones (Timezone)

type Input =
    { organization :: OrganizationNW
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , nameError :: Boolean
    , websiteError :: Boolean
    }

type Output =
    { organization :: OrganizationNW
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    }

type State = Input

data Action
    = Initialize
    | Receive Input
    | UpdateOrganization Organization
    | UpdateName String
    | UpdateWebsite (Maybe String)
    | UpdateAgeFrom (Maybe Int)
    | UpdateAgeTo (Maybe Int)
    | UpdateLocations (Array String)
    | UpdateLanguages (Array String)
    | UpdateMicrophone Boolean
    | UpdateTimezone (Maybe String)
    | UpdateWeekdayFrom (Maybe String)
    | UpdateWeekdayTo (Maybe String)
    | UpdateWeekendFrom (Maybe String)
    | UpdateWeekendTo (Maybe String)

type ChildSlots =
    ( location :: MultiTreeSelect.Slot String
    , language :: MultiSelect.Slot String Unit
    , timezone :: SingleSelect.Slot Timezone Unit
    , organizationInfo :: OrganizationInfo.Slot
    )

type Slot = H.Slot (Const Void) Output Unit

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render state =
    HH.div_ $
    [ HH.h2 [ HS.class_ "platform-id-heading" ]
        [ HH.text "General"
        , organizationRadioBadges (fromOrganizationNW state.organization) UpdateOrganization
        , organizationInfo
        ]
    ]
    <>
    ( case state.organization of
        InformalNW -> []
        OrganizedNW { name, website } -> Array.singleton $
            responsiveInputGroups
            [ nameInputGroup name UpdateName state.nameError
            , websiteInputGroup website UpdateWebsite state.websiteError
            ]
    )
    <>
    [ inputGroupsHeading "Personal"
    , responsiveInputGroups
        [ ageInputGroup state.ageFrom state.ageTo UpdateAgeFrom UpdateAgeTo
        , locationInputGroup state.locations UpdateLocations
        ]
    , inputGroupsHeading "Communication"
    , responsiveInputGroups
        [ languagesInputGroup state.languages UpdateLanguages
        , microphoneInputGroup state.microphone UpdateMicrophone
        ]
    , inputGroupsHeading "Time available"
    , responsiveInputGroups
        [ timezoneInputGroup state.timezone UpdateTimezone
        , timeRangeInputGroup "Online on weekdays" (isNothing state.timezone)
            state.weekdayFrom state.weekdayTo UpdateWeekdayFrom UpdateWeekdayTo
        , timeRangeInputGroup "Online on weekends" (isNothing state.timezone)
            state.weekendFrom state.weekendTo UpdateWeekendFrom UpdateWeekendTo
        ]
    ]

raiseOutput :: forall left. State -> H.HalogenM State Action ChildSlots Output (Async left) Unit
raiseOutput = pick >>> H.raise

handleAction :: forall left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    timezone <- maybe getClientTimezone pure state.timezone
    newState <- H.modify _ { timezone = Just timezone }
    raiseOutput newState
handleAction (Receive input) =
    H.put input
handleAction (UpdateOrganization organization) = do
    state <- H.get
    if fromOrganizationNW state.organization == organization
    then pure unit
    else do
        state' <- H.modify _ { organization = toOrganizationNW organization }
        raiseOutput state'
handleAction (UpdateName name) = do
    state <- H.modify
        case _ of
        state @ { organization: InformalNW } -> state
        state @ { organization: OrganizedNW state' } ->
            state { organization = OrganizedNW state' { name = name } }
    raiseOutput state
handleAction (UpdateWebsite website) = do
    state <- H.modify
        case _ of
        state @ { organization: InformalNW } -> state
        state @ { organization: OrganizedNW state' } ->
            state { organization = OrganizedNW state' { website = website } }
    raiseOutput state
handleAction (UpdateAgeFrom ageFrom) = do
    state <- H.modify _ { ageFrom = ageFrom }
    raiseOutput state
handleAction (UpdateAgeTo ageTo) = do
    state <- H.modify _ { ageTo = ageTo }
    raiseOutput state
handleAction (UpdateLocations locations) = do
    state <- H.modify _ { locations = locations }
    raiseOutput state
handleAction (UpdateLanguages languages) = do
    state <- H.modify _ { languages = languages }
    raiseOutput state
handleAction (UpdateMicrophone microphone) = do
    state <- H.modify _ { microphone = microphone }
    raiseOutput state
handleAction (UpdateTimezone timezone) = do
    state <- H.modify _ { timezone = timezone }
    raiseOutput state
handleAction (UpdateWeekdayFrom weekdayFrom) = do
    state <- H.modify _ { weekdayFrom = weekdayFrom }
    raiseOutput state
handleAction (UpdateWeekdayTo weekdayTo) = do
    state <- H.modify _ { weekdayTo = weekdayTo }
    raiseOutput state
handleAction (UpdateWeekendFrom weekendFrom) = do
    state <- H.modify _ { weekendFrom = weekendFrom }
    raiseOutput state
handleAction (UpdateWeekendTo weekendTo) = do
    state <- H.modify _ { weekendTo = weekendTo }
    raiseOutput state

component :: forall query left. H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

emptyInput :: Input
emptyInput =
    { organization: InformalNW
    , ageFrom: Nothing
    , ageTo: Nothing
    , locations: []
    , languages: []
    , microphone: false
    , timezone: Nothing
    , weekdayFrom: Nothing
    , weekdayTo: Nothing
    , weekendFrom: Nothing
    , weekendTo: Nothing
    , nameError: false
    , websiteError: false
    }

teamFormInput
    :: forall action children left
    .  Input
    -> (Output -> Maybe action)
    -> HH.ComponentHTML action (teamFormInput :: Slot | children) (Async left)
teamFormInput input handleMessage =
    HH.slot (SProxy :: SProxy "teamFormInput") unit component input handleMessage
