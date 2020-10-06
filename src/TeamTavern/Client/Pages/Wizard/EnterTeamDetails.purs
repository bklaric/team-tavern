module TeamTavern.Client.Pages.Wizard.EnterTeamDetails where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (unwrap)
import Data.String as String
import Data.Variant (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.SelectDefinitive.MultiSelect (multiSelect)
import TeamTavern.Client.Components.SelectDefinitive.MultiSelect as MultiSelect
import TeamTavern.Client.Components.SelectDefinitive.MultiTreeSelect (multiTreeSelect)
import TeamTavern.Client.Components.SelectDefinitive.MultiTreeSelect as MultiTreeSelect
import TeamTavern.Client.Components.SelectDefinitive.SingleSelect (singleSelect)
import TeamTavern.Client.Components.SelectDefinitive.SingleSelect as SingleSelect
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass)
import TeamTavern.Server.Infrastructure.Languages (allLanguages)
import TeamTavern.Server.Infrastructure.Regions (Region(..), allRegions)
import TeamTavern.Server.Infrastructure.Timezones (Timezone, allTimezones)

type Input =
    { name :: String
    , website :: String
    , ageFrom :: String
    , ageTo :: String
    , locations :: Array String
    , languages :: Array String
    , hasMicrophone :: Boolean
    , discordServer :: String
    , timezone :: Maybe String
    , weekdayFrom :: String
    , weekdayTo :: String
    , weekendFrom :: String
    , weekendTo :: String
    , about :: String
    , nameError :: Boolean
    , websiteError :: Boolean
    , discordServerError :: Boolean
    , aboutError :: Boolean
    }

type Output =
    { name :: String
    , website :: String
    , ageFrom :: String
    , ageTo :: String
    , locations :: Array String
    , languages :: Array String
    , hasMicrophone :: Boolean
    , discordServer :: String
    , timezone :: Maybe String
    , weekdayFrom :: String
    , weekdayTo :: String
    , weekendFrom :: String
    , weekendTo :: String
    , about :: String
    }

type State =
    { name :: String
    , website :: String
    , ageFrom :: String
    , ageTo :: String
    , locations :: Array String
    , languages :: Array String
    , hasMicrophone :: Boolean
    , discordServer :: String
    , timezone :: Maybe String
    , weekdayFrom :: String
    , weekdayTo :: String
    , weekendFrom :: String
    , weekendTo :: String
    , about :: String
    , nameError :: Boolean
    , websiteError :: Boolean
    , discordServerError :: Boolean
    , aboutError :: Boolean
    }

data Action
    = UpdateName String
    | UpdateWebsite String
    | UpdateAgeFrom String
    | UpdateAgeTo String
    | UpdateLocations (Array String)
    | UpdateLanguages (Array String)
    | UpdateHasMicrophone Boolean
    | UpdateDiscordServer String
    | UpdateTimezone (Maybe String)
    | UpdateWeekdayFrom String
    | UpdateWeekdayTo String
    | UpdateWeekendFrom String
    | UpdateWeekendTo String
    | UpdateAbout String

type ChildSlots =
    ( location :: MultiTreeSelect.Slot String
    , language :: MultiSelect.Slot String Unit
    , timezone :: SingleSelect.Slot Timezone Unit
    )

type Slot = H.Slot (Const Void) Output Unit

regionToOption :: Region -> MultiTreeSelect.InputEntry String
regionToOption (Region region subRegions) = MultiTreeSelect.InputEntry
    { option: region
    , subEntries: subRegions <#> regionToOption
    }

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render state =
    HH.div_
    [ HH.h3 [ HS.class_ "input-groups-heading" ]
        [ HH.text "General" ]
    , HH.div [ HS.class_ "responsive-input-groups" ]
        [ HH.div [ HS.class_ "input-group" ]
            [ HH.label
                [ HS.class_ "input-label"]
                [ HH.i [ HS.class_ "fas fa-signature filter-field-icon" ] []
                , HH.span [ HS.class_ "filter-field-label" ] [ HH.text "Name" ]
                , divider
                , HH.span [ HP.class_ $ H.ClassName "input-primary-sublabel" ] [ HH.text "required" ]
                ]
            , HH.input
                [ HS.class_ "text-line-input"
                , HP.type_ HP.InputText
                , HP.value state.name
                , HE.onValueInput $ Just <<< UpdateName
                ]
            , HH.p
                [ HS.class_ $ unwrap $ inputErrorClass state.nameError ]
                [ HH.text $ "Your name is all fucked up, what's wrong with you." ]
            ]
        , HH.div [ HS.class_ "input-group" ]
            [ HH.label
                [ HS.class_ "input-label"]
                [ HH.i [ HS.class_ "fas fa-globe filter-field-icon" ] []
                , HH.span [ HS.class_ "filter-field-label" ] [ HH.text "Website" ]
                ]
            , HH.input
                [ HS.class_ "text-line-input"
                , HP.type_ HP.InputText
                , HP.value state.website
                , HE.onValueInput $ Just <<< UpdateWebsite
                ]
            , HH.p
                [ HS.class_ $ unwrap $ inputErrorClass state.nameError ]
                [ HH.text $ "This isn't a valid website, you moron." ]
            ]
        ]
    , HH.h3 [ HS.class_ "input-groups-heading" ]
        [ HH.text "Personal" ]
    , HH.div [ HS.class_ "responsive-input-groups" ]
        [ HH.div [ HS.class_ "input-group" ]
            [ HH.label
                [ HS.class_ "input-label" ]
                [ HH.i [ HS.class_ "fas fa-calendar-alt filter-field-icon" ] []
                , HH.span [ HS.class_ "filter-field-label" ] [ HH.text "Age" ]
                ]
            , HH.div [ HS.class_ "timespan-group" ]
                [ HH.span [ HS.class_ "timespan-group-from" ] [ HH.text "From" ]
                , HH.input
                    [ HS.class_ $ "text-line-input timespan-group-input"
                    , HP.type_ HP.InputNumber
                    , HP.value state.ageFrom
                    , HE.onValueChange $ Just <<< UpdateAgeFrom
                    ]
                , HH.span [ HS.class_ "timespan-group-to" ] [ HH.text "to" ]
                , HH.input
                    [ HS.class_ $ "text-line-input timespan-group-input"
                    , HP.type_ HP.InputNumber
                    , HP.value state.ageTo
                    , HE.onValueChange $ Just <<< UpdateAgeTo
                    ]
                ]
            ]
        , HH.div [ HS.class_ "input-group" ]
            [ HH.label
                [ HS.class_ "input-label" ]
                [ HH.i [ HS.class_ "fas fa-globe-europe filter-field-icon" ] []
                , HH.span [ HS.class_ "filter-field-label" ] [ HH.text "Location" ]
                ]
            , multiTreeSelect (SProxy :: SProxy "location")
                { entries: allRegions <#> regionToOption
                , selected: state.locations
                , labeler: identity
                , comparer: (==)
                , filter: "Search locations"
                }
                \(MultiTreeSelect.SelectedChanged locations) -> Just $ UpdateLocations locations
            ]
        ]
    , HH.h3 [ HS.class_ "input-groups-heading" ]
        [ HH.text "Communication" ]
    , HH.div [ HS.class_ "responsive-input-groups" ]
        [ HH.div [ HS.class_ "input-group" ]
            [ HH.label
                [ HS.class_ "input-label" ]
                [ HH.i [ HS.class_ "fas fa-comments filter-field-icon" ] []
                , HH.span [ HS.class_ "filter-field-label" ] [ HH.text "Language" ]
                ]
            , multiSelect (SProxy :: SProxy "language")
                { options: allLanguages
                , selected: state.languages
                , labeler: identity
                , comparer: (==)
                , filter: Just "Search languages"
                }
                \(MultiSelect.SelectedChanged languages) -> Just $ UpdateLanguages languages
            ]
        , HH.div [ HS.class_ "input-group" ]
            [ HH.label
                [ HS.class_ "input-label" ]
                [ HH.i [ HS.class_ "fas fa-microphone filter-field-icon" ] []
                , HH.span [ HS.class_ "filter-field-label" ] [ HH.text "Microphone" ]
                ]
            , HH.label
                [ HS.class_ "checkbox-input-label" ]
                [ HH.input
                    [ HS.class_ "checkbox-input"
                    , HP.type_ HP.InputCheckbox
                    , HP.checked state.hasMicrophone
                    , HE.onChecked $ Just <<< UpdateHasMicrophone
                    ]
                , HH.text "Must have a microphone and be willing to communicate."
                ]
            ]
        , HH.div [ HS.class_ "input-group" ]
            [ HH.label
                [ HS.class_ "input-label"]
                [ HH.i [ HS.class_ "fab fa-discord filter-field-icon" ] []
                , HH.span [ HS.class_ "filter-field-label" ] [ HH.text "Discord server" ]
                ]
            , HH.input
                [ HS.class_ "text-line-input"
                , HP.type_ HP.InputText
                , HP.value state.discordServer
                , HE.onValueInput $ Just <<< UpdateDiscordServer
                ]
            , HH.label
                [ HS.class_ "input-underlabel" ]
                [ HH.text "Example: discord.gg/AbCdEfG" ]
            , HH.p
                [ HS.class_ $ unwrap $ inputErrorClass state.discordServerError ]
                [ HH.text $ "This does not look like a valid Discord tag." ]
            ]
        ]
    , HH.h3 [ HS.class_ "input-groups-heading" ]
        [ HH.text "Time available" ]
    , HH.div [ HS.class_ "responsive-input-groups" ]
        [ HH.div [ HS.class_ "input-group" ]
            [ HH.label
                [ HS.class_ "input-label" ]
                [ HH.i [ HS.class_ "fas fa-user-clock filter-field-icon" ] []
                , HH.span [ HS.class_ "filter-field-label" ] [ HH.text "Timezone" ]
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
        , HH.div [ HS.class_ "input-group" ] $
            [ HH.label
                [ HS.class_ "input-label" ]
                [ HH.i [ HS.class_ "fas fa-clock filter-field-icon" ] []
                , HH.span [ HS.class_ "filter-field-label" ] [ HH.text "Online on weekdays" ]
                ]
            , HH.div [ HS.class_ "timespan-group" ]
                [ HH.span [ HS.class_ "timespan-group-from" ] [ HH.text "From" ]
                , HH.input
                    [ HS.class_ $ "text-line-input timespan-group-input"
                    , HP.type_ HP.InputTime
                    , HP.disabled $ isNothing state.timezone
                    , HP.value state.weekdayFrom
                    , HE.onValueChange $ Just <<< UpdateWeekdayFrom
                    ]
                , HH.span [ HS.class_ "timespan-group-to" ] [ HH.text "to" ]
                , HH.input
                    [ HS.class_ $ "text-line-input timespan-group-input"
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
                    [ HS.class_ "input-underlabel" ]
                    [ HH.text $ "Set your timezone to unlock this field." ]
            else if String.null state.weekdayFrom && (not $ String.null state.weekdayTo)
                || (not $ String.null state.weekdayFrom) && String.null state.weekdayTo
            then Array.singleton $
                HH.label
                [ HS.class_ "input-underlabel" ]
                [ HH.text $ "Enter both times for the field to have effect."
                ]
            else [])
        , HH.div [ HS.class_ "input-group" ] $
            [ HH.label
                [ HS.class_ "input-label" ]
                [ HH.i [ HS.class_ "fas fa-clock filter-field-icon" ] []
                , HH.span [ HS.class_ "filter-field-label" ] [ HH.text "Online on weekends" ]
                ]
            , HH.div [ HS.class_ "timespan-group" ]
                [ HH.span [ HS.class_ "timespan-group-from" ] [ HH.text "From" ]
                , HH.input
                    [ HS.class_ $ "text-line-input timespan-group-input"
                    , HP.type_ HP.InputTime
                    , HP.disabled $ isNothing state.timezone
                    , HP.value state.weekendFrom
                    , HE.onValueChange $ Just <<< UpdateWeekendFrom
                    ]
                , HH.span [ HS.class_ "timespan-group-to" ] [ HH.text "to" ]
                , HH.input
                    [ HS.class_ $ "text-line-input timespan-group-input"
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
                    [ HS.class_ "input-underlabel" ]
                    [ HH.text $ "Set your timezone to unlock this field." ]
            else if String.null state.weekendFrom && (not $ String.null state.weekendTo)
                || (not $ String.null state.weekendFrom) && String.null state.weekendTo
            then Array.singleton $
                HH.label
                [ HS.class_ "input-underlabel" ]
                [ HH.text $ "Enter both times for the field to have effect."
                ]
            else [])
        ]
    , HH.h3 [ HS.class_ "input-groups-heading" ]
        [ HH.text "About" ]
    , HH.textarea
        [ HS.class_ "text-input"
        , HE.onValueInput $ Just <<< UpdateAbout
        , HP.value state.about
        ]
    , HH.label [ HS.class_ "input-underlabel" ]
        [ HH.text """Yo nigga, write about yourself. What are you like? Just how
            gay are you? What kind of faggots are you looking for?"""
        ]
    , HH.p
        [ HS.class_ $ unwrap $ inputErrorClass state.aboutError ]
        [ HH.text "About text cannot be more than 2000 characters long." ]
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
handleAction (UpdateHasMicrophone hasMicrophone) = do
    state <- H.modify _ { hasMicrophone = hasMicrophone }
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
        { handleAction = handleAction }
    }

emptyInput :: Input
emptyInput =
    { name: ""
    , website: ""
    , ageFrom: ""
    , ageTo: ""
    , locations: []
    , languages: []
    , hasMicrophone: false
    , discordServer: ""
    , timezone: Nothing
    , weekdayFrom: ""
    , weekdayTo: ""
    , weekendFrom: ""
    , weekendTo: ""
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
