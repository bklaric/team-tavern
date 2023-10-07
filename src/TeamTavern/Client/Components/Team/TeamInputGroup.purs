module TeamTavern.Client.Components.Team.TeamInputGroup where

import Prelude

import Async (Async)
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Halogen.HTML (HTML, ComponentHTML)
import TeamTavern.Client.Components.Input (checkboxInput, inputError, inputGroup, inputLabel, inputUnderlabel, numberRangeInput, requiredInputLabel, requiredTextLineInput, textLineInput)
import TeamTavern.Client.Components.Select.MultiSelect (multiSelect)
import TeamTavern.Client.Components.Select.MultiSelect as MultiSelect
import TeamTavern.Client.Components.Select.MultiTreeSelect (multiTreeSelect)
import TeamTavern.Client.Components.Select.MultiTreeSelect as MultiTreeSelect
import TeamTavern.Shared.Languages (allLanguages)
import TeamTavern.Shared.Regions (Region(..), allRegions)

locationToEntry :: Region -> MultiTreeSelect.InputEntry String
locationToEntry (Region region subRegions) =
    MultiTreeSelect.InputEntry
    { option: region
    , subEntries: subRegions <#> locationToEntry
    }

nameInputGroup :: ∀ slots action. String -> (String -> action) -> Boolean -> HTML slots action
nameInputGroup value onValue error =
    inputGroup $
    [ requiredInputLabel "fas fa-signature" "Name"
    , requiredTextLineInput value onValue
    ]
    <>
    inputError error "Name is required and cannot be more than 40 characters long."

websiteInputGroup :: ∀ slots action.
    Maybe String -> (Maybe String -> action) -> Boolean -> HTML slots action
websiteInputGroup value onValue error =
    inputGroup $
    [ inputLabel "fas fa-globe" "Website"
    , textLineInput value onValue
    ]
    <>
    inputError error ("Website must be valid and cannot be more than 200 characters long. "
        <> "Please check and try again.")

ageInputGroup :: ∀ slots action.
    Maybe Int -> Maybe Int -> (Maybe Int -> action) -> (Maybe Int -> action) -> HTML slots action
ageInputGroup ageFrom ageTo onAgeFrom onAgeTo =
    inputGroup
    [ inputLabel "fas fa-calendar-alt" "Age"
    , numberRangeInput ageFrom ageTo onAgeFrom onAgeTo
    , inputUnderlabel "You can enter either one or both of the age limits."
    ]

locationInputGroup
    :: ∀ slots action left. Array String
    -> (Array String -> action)
    -> ComponentHTML action (location :: MultiTreeSelect.Slot String | slots) (Async left)
locationInputGroup value onValue =
    inputGroup
    [ inputLabel "fas fa-globe-europe" "Location"
    , multiTreeSelect (Proxy :: _ "location")
        { entries: allRegions <#> locationToEntry
        , selected: value
        , labeler: identity
        , comparer: (==)
        , filter: "Search locations"
        }
        onValue
    ]

languagesInputGroup
    :: ∀ slots action left
    .  Array String
    -> (Array String -> action)
    -> ComponentHTML action (language :: MultiSelect.Slot String Unit | slots) (Async left)
languagesInputGroup value onValue =
    inputGroup
    [ inputLabel "fas fa-comments" "Language"
    , multiSelect (Proxy :: _ "language")
        { options: allLanguages
        , selected: value
        , labeler: identity
        , comparer: (==)
        , filter: Just "Search languages"
        }
        onValue
    ]

microphoneInputGroup :: ∀ slots action. Boolean -> (Boolean -> action) -> HTML slots action
microphoneInputGroup value onValue =
    inputGroup
    [ inputLabel "fas fa-microphone" "Microphone"
    , checkboxInput value onValue "Must have a microphone and be willing to communicate."
    ]

discordServerInputGroup :: ∀ slots action.
    Maybe String -> (Maybe String -> action) -> Boolean -> HTML slots action
discordServerInputGroup value onValue error =
    inputGroup $
    [ inputLabel "fab fa-discord" "Discord server"
    , textLineInput value onValue
    , inputUnderlabel "Example: discord.gg/AbCdEfG"
    ]
    <>
    inputError error "This does not look like a valid Discord server."
