module TeamTavern.Client.Components.Player.PlayerFormInput (Input, Output, Slot, emptyInput, playerFormInput) where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Date as Date
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Variant (SProxy(..))
import Effect.Now (nowDate)
import Halogen as H
import Halogen.HTML as HH
import Record as Record
import Record.Extra (pick)
import TeamTavern.Client.Components.Input (inputGroupsHeading, responsiveInputGroups)
import TeamTavern.Client.Components.InputGroup (timeRangeInputGroup, timezoneInputGroup)
import TeamTavern.Client.Components.Player.PlayerInputGroup (aboutInputGroup, birthdayInputGroup, discordTagInputGroup, languagesInputGroup, locationInputGroup, microphoneInputGroup)
import TeamTavern.Client.Components.Select.MultiSelect as MultiSelect
import TeamTavern.Client.Components.Select.SingleSelect as SingleSelect
import TeamTavern.Client.Components.Select.SingleTreeSelect as SingleTreeSelect
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Server.Infrastructure.Timezones (Timezone)

type Input =
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
    , about :: String
    , discordTagError :: Boolean
    , aboutError :: Boolean
    }

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
    , about :: String
    }

type State =
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
    , about :: String
    , thirteenYearsAgo :: String
    , discordTagError :: Boolean
    , aboutError :: Boolean
    }

data Action
    = Initialize
    | Receive Input
    | UpdateBirthday (Maybe String)
    | UpdateLocation (Maybe String)
    | UpdateLanguages (Array String)
    | UpdateMicrophone Boolean
    | UpdateDiscordTag (Maybe String)
    | UpdateTimezone (Maybe String)
    | UpdateWeekdayFrom (Maybe String)
    | UpdateWeekdayTo (Maybe String)
    | UpdateWeekendFrom (Maybe String)
    | UpdateWeekendTo (Maybe String)
    | UpdateAbout String

type ChildSlots =
    ( location :: SingleTreeSelect.Slot String
    , language :: MultiSelect.Slot String Unit
    , timezone :: SingleSelect.Slot Timezone Unit
    )

type Slot = H.Slot (Const Void) Output Unit

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render state =
    HH.div_
    [ inputGroupsHeading "Contact"
    , responsiveInputGroups
        [ discordTagInputGroup state.discordTag UpdateDiscordTag state.discordTagError ]
    , inputGroupsHeading "Personal"
    , responsiveInputGroups
        [ birthdayInputGroup state.thirteenYearsAgo state.birthday UpdateBirthday
        , locationInputGroup state.location UpdateLocation
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
    , inputGroupsHeading "About"
    , aboutInputGroup state.about UpdateAbout state.aboutError
    ]

raiseOutput :: forall left. State -> H.HalogenM State Action ChildSlots Output (Async left) Unit
raiseOutput = pick >>> H.raise

handleAction :: forall left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    timezone <- maybe getClientTimezone pure state.timezone
    now <- H.liftEffect nowDate
    let year = now # Date.year # fromEnum # (_ - 13)
    let month = fromEnum $ Date.month now
    let day = fromEnum $ Date.day now
    let thirteenYearsAgo
            =  show year <> "-"
            <> (if month < 10 then "0" <> show month else show month) <> "-"
            <> (if day < 10 then "0" <> show day else show day)
    newState <- H.modify _
        { timezone = Just timezone
        , thirteenYearsAgo = thirteenYearsAgo
        }
    raiseOutput newState
handleAction (Receive input) =
    H.modify_ \state ->
        input # Record.insert (SProxy :: SProxy "thirteenYearsAgo") state.thirteenYearsAgo
handleAction (UpdateBirthday birthday) = do
    state <- H.modify _ { birthday = birthday }
    raiseOutput state
handleAction (UpdateLocation location) = do
    state <- H.modify _ { location = location }
    raiseOutput state
handleAction (UpdateLanguages languages) = do
    state <- H.modify _ { languages = languages }
    raiseOutput state
handleAction (UpdateMicrophone microphone) = do
    state <- H.modify _ { microphone = microphone }
    raiseOutput state
handleAction (UpdateDiscordTag discordTag) = do
    state <- H.modify _ { discordTag = discordTag }
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
handleAction (UpdateAbout about) = do
    state <- H.modify _ { about = about }
    raiseOutput state

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: Record.insert (SProxy :: SProxy "thirteenYearsAgo") ""
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

emptyInput :: Input
emptyInput =
    { birthday: Nothing
    , location: Nothing
    , languages: []
    , microphone: false
    , discordTag: Nothing
    , timezone: Nothing
    , weekdayFrom: Nothing
    , weekdayTo: Nothing
    , weekendFrom: Nothing
    , weekendTo: Nothing
    , about: ""
    , discordTagError: false
    , aboutError: false
    }

playerFormInput
    :: forall action children left
    .  Input
    -> (Output -> Maybe action)
    -> HH.ComponentHTML action (playerFormInput :: Slot | children) (Async left)
playerFormInput input handleMessage =
    HH.slot (SProxy :: SProxy "playerFormInput") unit component input handleMessage
