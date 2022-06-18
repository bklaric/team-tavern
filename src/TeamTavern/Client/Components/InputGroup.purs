module TeamTavern.Client.Components.InputGroup where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Halogen.HTML (HTML, ComponentHTML)
import TeamTavern.Client.Components.Input (inputGroup, inputLabel, timeRangeInput, timeRangeInputUnderlabel)
import TeamTavern.Client.Components.Select.SingleSelect (singleSelect)
import TeamTavern.Client.Components.Select.SingleSelect as SingleSelect
import TeamTavern.Shared.Timezones (Timezone, allTimezones)

timezoneInputGroup
    :: forall slots action left
    .  Maybe String
    -> (Maybe String -> action)
    -> ComponentHTML action (timezone :: SingleSelect.Slot Timezone Unit | slots) (Async left)
timezoneInputGroup value onValue =
    inputGroup
    [ inputLabel "fas fa-user-clock" "Timezone"
    , singleSelect (Proxy :: _ "timezone")
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
        \timezone -> onValue (timezone <#> _.name)
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
