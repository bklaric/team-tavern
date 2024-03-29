module TeamTavern.Client.Components.Player.PlayerInputGroup where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Type.Proxy (Proxy(..))
import Halogen (ComponentHTML, Slot)
import Halogen.HTML (HTML)
import TeamTavern.Client.Components.Input (checkboxInput, dateInput, inputError, inputGroup, inputLabel, inputUnderlabel, textLineInput)
import TeamTavern.Client.Components.Select.MultiSelect (multiSelect)
import TeamTavern.Client.Components.Select.SingleTreeSelect (singleTreeSelect)
import TeamTavern.Client.Components.Select.SingleTreeSelect as SingleTreeSelect
import TeamTavern.Shared.Languages (allLanguages)
import TeamTavern.Shared.Regions (Region(..), allRegions)

locationToEntry :: Region -> SingleTreeSelect.InputEntry String
locationToEntry (Region region subRegions) =
    SingleTreeSelect.InputEntry
    { option: region
    , subEntries: subRegions <#> locationToEntry
    }

birthdayInputGroup :: ∀ slots action.
    String -> Maybe String -> (Maybe String -> action) -> HTML slots action
birthdayInputGroup max value onValue =
    inputGroup
    [ inputLabel "fas fa-calendar-alt" "Birthday"
    , dateInput "1900-01-01" max value onValue
    , inputUnderlabel
        $  "Your birthday will be used to calculate your age "
        <> "and will not be shown to anyone."
    ]

locationInputGroup
    :: ∀ slots action left
    .  Maybe String
    -> (Maybe String -> action)
    -> ComponentHTML action (location :: Slot (Const Void) (Maybe String) Unit | slots) (Async left)
locationInputGroup value onValue =
    inputGroup
    [ inputLabel "fas fa-globe-europe" "Location"
    , singleTreeSelect (Proxy :: _ "location")
        { entries: allRegions <#> locationToEntry
        , selected: value
        , labeler: identity
        , comparer: (==)
        , filter: "Search locations"
        }
        onValue
    , inputUnderlabel
        $  "You can select either a specific country or "
        <> "one of the containing regions."
    ]

languagesInputGroup
    :: ∀ slots action left
    .  Array String
    -> (Array String -> action)
    -> ComponentHTML action (language :: Slot (Const Void) (Array String) Unit | slots) (Async left)
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
    , checkboxInput value onValue "I have a microphone and I'm willing to communicate."
    ]

discordTagInputGroup :: ∀ slots action.
    Maybe String -> (Maybe String -> action) -> Boolean -> HTML slots action
discordTagInputGroup value onValue error =
    inputGroup $
    [ inputLabel "fab fa-discord" "Discord username or tag"
    , textLineInput value onValue
    , inputUnderlabel "Example: username or username#1234"
    ]
    <> inputError error "This does not look like a valid Discord tag."
