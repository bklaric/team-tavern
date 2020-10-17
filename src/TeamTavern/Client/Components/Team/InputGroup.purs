module TeamTavern.Client.Components.Team.InputGroup where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..))
import Halogen.HTML (HTML, ComponentHTML)
import TeamTavern.Client.Components.Input (checkboxInput, inputError, inputGroup, inputLabel, inputUnderlabel, numberRangeInput, requiredInputLabel, requiredTextLineInput, textInput_, textLineInput, timeRangeInput, timeRangeInputUnderlabel)
import TeamTavern.Client.Components.SelectDefinitive.MultiSelect (multiSelect)
import TeamTavern.Client.Components.SelectDefinitive.MultiSelect as MultiSelect
import TeamTavern.Client.Components.SelectDefinitive.MultiTreeSelect (multiTreeSelect)
import TeamTavern.Client.Components.SelectDefinitive.MultiTreeSelect as MultiTreeSelect
import TeamTavern.Client.Components.SelectDefinitive.SingleSelect (singleSelect)
import TeamTavern.Client.Components.SelectDefinitive.SingleSelect as SingleSelect
import TeamTavern.Server.Infrastructure.Languages (allLanguages)
import TeamTavern.Server.Infrastructure.Regions (Region(..), allRegions)
import TeamTavern.Server.Infrastructure.Timezones (Timezone, allTimezones)

locationToEntry :: Region -> MultiTreeSelect.InputEntry String
locationToEntry (Region region subRegions) = MultiTreeSelect.InputEntry
    { option: region
    , subEntries: subRegions <#> locationToEntry
    }

nameInputGroup :: forall slots action. String -> (String -> action) -> Boolean -> HTML slots action
nameInputGroup value onValue error =
    inputGroup $
    [ requiredInputLabel "fas fa-signature" "Name"
    , requiredTextLineInput value onValue
    ]
    <>
    inputError error "Name isn't valid, kek."

websiteInputGroup :: forall slots action.
    Maybe String -> (Maybe String -> action) -> Boolean -> HTML slots action
websiteInputGroup value onValue error =
    inputGroup $
    [ inputLabel "fas fa-globe" "Website"
    , textLineInput value onValue
    ]
    <>
    inputError error "Website isn't valid, kek."

ageInputGroup :: forall slots action.
    Maybe Int -> Maybe Int -> (Maybe Int -> action) -> (Maybe Int -> action) -> HTML slots action
ageInputGroup ageFrom ageTo onAgeFrom onAgeTo =
    inputGroup
    [ inputLabel "fas fa-calendar-alt" "Age"
    , numberRangeInput ageFrom ageTo onAgeFrom onAgeTo
    ]

locationInputGroup
    :: forall slots action left. Array String
    -> (Array String -> action)
    -> ComponentHTML action (location :: MultiTreeSelect.Slot String | slots) (Async left)
locationInputGroup value onValue =
    inputGroup
    [ inputLabel "fas fa-globe-europe" "Location"
    , multiTreeSelect (SProxy :: SProxy "location")
        { entries: allRegions <#> locationToEntry
        , selected: value
        , labeler: identity
        , comparer: (==)
        , filter: "Search locations"
        }
        \(MultiTreeSelect.SelectedChanged locations) -> Just $ onValue locations
    ]

languagesInputGroup
    :: forall slots action left
    .  Array String
    -> (Array String -> action)
    -> ComponentHTML action (language :: MultiSelect.Slot String Unit | slots) (Async left)
languagesInputGroup value onValue =
    inputGroup
    [ inputLabel "fas fa-comments" "Languages"
    , multiSelect (SProxy :: SProxy "language")
        { options: allLanguages
        , selected: value
        , labeler: identity
        , comparer: (==)
        , filter: Just "Search languages"
        }
        \(MultiSelect.SelectedChanged languages) -> Just $ onValue languages
    ]

microphoneInputGroup :: forall slots action. Boolean -> (Boolean -> action) -> HTML slots action
microphoneInputGroup value onValue =
    inputGroup
    [ inputLabel "fas fa-microphone" "Microphone"
    , checkboxInput value onValue "Must have a microphone and be willing to communicate."
    ]

discordServerInputGroup :: forall slots action.
    Maybe String -> (Maybe String -> action) -> Boolean -> HTML slots action
discordServerInputGroup value onValue error =
    inputGroup $
    [ inputLabel "fab fa-discord" "Discord server"
    , textLineInput value onValue
    , inputUnderlabel "Example: discord.gg/AbCdEfG"
    ]
    <>
    inputError error "This does not look like a valid Discord tag."

timezoneInputGroup
    :: forall slots action left
    .  Maybe String
    -> (Maybe String -> action)
    -> ComponentHTML action (timezone :: SingleSelect.Slot Timezone Unit | slots) (Async left)
timezoneInputGroup value onValue =
    inputGroup
    [ inputLabel "fas fa-user-clock" "Timezone"
    , singleSelect (SProxy :: SProxy "timezone")
        { options: allTimezones # Array.sortBy \leftTimezone rightTimezone -> let
            countryComparison =
                leftTimezone.country `compare` rightTimezone.country
            in
            case countryComparison of
            EQ -> leftTimezone.city `compare` rightTimezone.city
            other -> other
        , selected: value >>= \timezone ->
            allTimezones # Array.find (_.name >>> (_ == timezone))
        , labeler: \{ city, country } ->
            country <> ", " <> city
        , comparer: \leftTimezone rightTimezone ->
            leftTimezone.name == rightTimezone.name
        , filter: Just "Search timezones"
        }
        \(SingleSelect.SelectedChanged timezone) -> Just $ onValue (timezone <#> _.name)
    ]

timeRangeInputGroup
    :: forall slots action
    .  String
    -> Boolean
    -> Maybe String
    -> Maybe String
    -> (Maybe String -> action)
    -> (Maybe String -> action)
    -> HTML slots action
timeRangeInputGroup label disabled fromValue toValue onFromValue onToValue =
    inputGroup $
    [ inputLabel "fas fa-clock" label
    , timeRangeInput disabled fromValue toValue onFromValue onToValue
    ]
    <>
    timeRangeInputUnderlabel disabled fromValue toValue

aboutInputGroup :: forall slots action. String -> (String -> action) -> Boolean -> HTML slots action
aboutInputGroup value onValue error =
    inputGroup $
    [ textInput_ value onValue
    , inputUnderlabel """Yo nigga, write about yourself. What are you like? Just how
        gay are you? What kind of faggots are you looking for?"""
    ]
    <>
    inputError error "About text cannot be more than 2000 characters long."
