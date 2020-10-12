module TeamTavern.Client.Pages.Wizard.EnterTeamDetails (Input, Output, Slot, emptyInput, enterTeamDetails) where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..), isNothing)
import Data.Variant (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Record as Record
import TeamTavern.Client.Components.Input (inputGroupsHeading, responsiveInputGroups)
import TeamTavern.Client.Components.SelectDefinitive.MultiSelect as MultiSelect
import TeamTavern.Client.Components.SelectDefinitive.MultiTreeSelect as MultiTreeSelect
import TeamTavern.Client.Components.SelectDefinitive.SingleSelect as SingleSelect
import TeamTavern.Client.Components.Team.InputGroup (aboutInputGroup, ageInputGroup, discordServerInputGroup, languagesInputGroup, locationInputGroup, microphoneInputGroup, nameInputGroup, timeRangeInputGroup, timezoneInputGroup, websiteInputGroup)
import TeamTavern.Server.Infrastructure.Timezones (Timezone)

type Input =
    { name :: String
    , website :: String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , discordServer :: String
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , about :: String
    , nameError :: Boolean
    , websiteError :: Boolean
    , discordServerError :: Boolean
    , aboutError :: Boolean
    }

type Output =
    { name :: String
    , website :: String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , locations :: Array String
    , languages :: Array String
    , microphone :: Boolean
    , discordServer :: String
    , timezone :: Maybe String
    , weekdayFrom :: Maybe String
    , weekdayTo :: Maybe String
    , weekendFrom :: Maybe String
    , weekendTo :: Maybe String
    , about :: String
    }

type State = Input

data Action
    = Receive Input
    | UpdateName String
    | UpdateWebsite String
    | UpdateAgeFrom (Maybe Int)
    | UpdateAgeTo (Maybe Int)
    | UpdateLocations (Array String)
    | UpdateLanguages (Array String)
    | UpdateMicrophone Boolean
    | UpdateDiscordServer String
    | UpdateTimezone (Maybe String)
    | UpdateWeekdayFrom (Maybe String)
    | UpdateWeekdayTo (Maybe String)
    | UpdateWeekendFrom (Maybe String)
    | UpdateWeekendTo (Maybe String)
    | UpdateAbout String

type ChildSlots =
    ( location :: MultiTreeSelect.Slot String
    , language :: MultiSelect.Slot String Unit
    , timezone :: SingleSelect.Slot Timezone Unit
    )

type Slot = H.Slot (Const Void) Output Unit

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render state =
    HH.div_
    [ inputGroupsHeading "General"
    , responsiveInputGroups
        [ nameInputGroup state.name UpdateName state.nameError
        , websiteInputGroup state.website UpdateWebsite state.websiteError
        ]
    , inputGroupsHeading "Personal"
    , responsiveInputGroups
        [ ageInputGroup state.ageFrom state.ageTo UpdateAgeFrom UpdateAgeTo
        , locationInputGroup state.locations UpdateLocations
        ]
    , inputGroupsHeading "Communication"
    , responsiveInputGroups
        [ languagesInputGroup state.languages UpdateLanguages
        , microphoneInputGroup state.microphone UpdateMicrophone
        , discordServerInputGroup state.discordServer UpdateDiscordServer state.discordServerError
        ]
    , inputGroupsHeading "Time available"
    , responsiveInputGroups
        [ timezoneInputGroup state.timezone UpdateTimezone
        , timeRangeInputGroup "Online on weekdays" (isNothing state.timezone)
            state.weekdayFrom state.weekdayTo UpdateWeekdayFrom UpdateWeekdayTo
        , timeRangeInputGroup "Online on weekends" (isNothing state.timezone)
            state.weekendFrom state.weekendTo UpdateWeekendFrom UpdateWeekendTo
        ]
    , inputGroupsHeading "About"
    , aboutInputGroup state.about UpdateAbout state.aboutError
    ]

stateToOutput :: State -> Output
stateToOutput state =
    state
    # Record.delete (SProxy :: SProxy "nameError")
    # Record.delete (SProxy :: SProxy "websiteError")
    # Record.delete (SProxy :: SProxy "discordServerError")
    # Record.delete (SProxy :: SProxy "aboutError")

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive input) =
    H.put input
handleAction (UpdateName name) = do
    state <- H.modify _ { name = name }
    H.raise $ stateToOutput state
handleAction (UpdateWebsite website) = do
    state <- H.modify _ { website = website }
    H.raise $ stateToOutput state
handleAction (UpdateAgeFrom ageFrom) = do
    state <- H.modify _ { ageFrom = ageFrom }
    H.raise $ stateToOutput state
handleAction (UpdateAgeTo ageTo) = do
    state <- H.modify _ { ageTo = ageTo }
    H.raise $ stateToOutput state
handleAction (UpdateLocations locations) = do
    state <- H.modify _ { locations = locations }
    H.raise $ stateToOutput state
handleAction (UpdateLanguages languages) = do
    state <- H.modify _ { languages = languages }
    H.raise $ stateToOutput state
handleAction (UpdateMicrophone microphone) = do
    state <- H.modify _ { microphone = microphone }
    H.raise $ stateToOutput state
handleAction (UpdateDiscordServer discordServer) = do
    state <- H.modify _ { discordServer = discordServer }
    H.raise $ stateToOutput state
handleAction (UpdateTimezone timezone) = do
    state <- H.modify _ { timezone = timezone }
    H.raise $ stateToOutput state
handleAction (UpdateWeekdayFrom weekdayFrom) = do
    state <- H.modify _ { weekdayFrom = weekdayFrom }
    H.raise $ stateToOutput state
handleAction (UpdateWeekdayTo weekdayTo) = do
    state <- H.modify _ { weekdayTo = weekdayTo }
    H.raise $ stateToOutput state
handleAction (UpdateWeekendFrom weekendFrom) = do
    state <- H.modify _ { weekendFrom = weekendFrom }
    H.raise $ stateToOutput state
handleAction (UpdateWeekendTo weekendTo) = do
    state <- H.modify _ { weekendTo = weekendTo }
    H.raise $ stateToOutput state
handleAction (UpdateAbout about) = do
    state <- H.modify _ { about = about }
    H.raise $ stateToOutput state

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

emptyInput :: Input
emptyInput =
    { name: ""
    , website: ""
    , ageFrom: Nothing
    , ageTo: Nothing
    , locations: []
    , languages: []
    , microphone: false
    , discordServer: ""
    , timezone: Nothing
    , weekdayFrom: Nothing
    , weekdayTo: Nothing
    , weekendFrom: Nothing
    , weekendTo: Nothing
    , about: ""
    , nameError: false
    , websiteError: false
    , discordServerError: false
    , aboutError: false
    }

enterTeamDetails
    :: forall action children left
    .  Input
    -> (Output -> Maybe action)
    -> HH.ComponentHTML action (enterTeamDetails :: Slot | children) (Async left)
enterTeamDetails input handleMessage =
    HH.slot (SProxy :: SProxy "enterTeamDetails") unit component input handleMessage
