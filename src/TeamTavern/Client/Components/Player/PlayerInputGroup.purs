module TeamTavern.Client.Components.Player.PlayerInputGroup where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..))
import Halogen (ComponentHTML, Slot)
import Halogen.HTML (HTML)
import TeamTavern.Client.Components.Input (checkboxInput, dateInput, inputError, inputGroup, inputLabel, inputUnderlabel, textInput_, textLineInput)
import TeamTavern.Client.Components.Select.MultiSelect (multiSelect)
import TeamTavern.Client.Components.Select.SingleTreeSelect (singleTreeSelect)
import TeamTavern.Client.Components.Select.SingleTreeSelect as SingleTreeSelect
import TeamTavern.Server.Infrastructure.Languages (allLanguages)
import TeamTavern.Server.Infrastructure.Regions (Region(..), allRegions)

locationToEntry :: Region -> SingleTreeSelect.InputEntry String
locationToEntry (Region region subRegions) =
    SingleTreeSelect.InputEntry
    { option: region
    , subEntries: subRegions <#> locationToEntry
    }

birthdayInputGroup :: forall slots action.
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
    :: forall slots action left
    .  Maybe String
    -> (Maybe String -> action)
    -> ComponentHTML action (location :: Slot (Const Void) (Maybe String) Unit | slots) (Async left)
locationInputGroup value onValue =
    inputGroup
    [ inputLabel "fas fa-globe-europe" "Location"
    , singleTreeSelect (SProxy :: SProxy "location")
        { entries: allRegions <#> locationToEntry
        , selected: value
        , labeler: identity
        , comparer: (==)
        , filter: "Search locations"
        }
        \location -> Just $ onValue location
    , inputUnderlabel
        $  "You can select either a specific country or "
        <> "one of the containing regions."
    ]

languagesInputGroup
    :: forall slots action left
    .  Array String
    -> (Array String -> action)
    -> ComponentHTML action (language :: Slot (Const Void) (Array String) Unit | slots) (Async left)
languagesInputGroup value onValue =
    inputGroup
    [ inputLabel "fas fa-comments" "Language"
    , multiSelect (SProxy :: SProxy "language")
        { options: allLanguages
        , selected: value
        , labeler: identity
        , comparer: (==)
        , filter: Just "Search languages"
        }
        \languages -> Just $ onValue languages
    ]

microphoneInputGroup :: forall slots action. Boolean -> (Boolean -> action) -> HTML slots action
microphoneInputGroup value onValue =
    inputGroup
    [ inputLabel "fas fa-microphone" "Microphone"
    , checkboxInput value onValue "I have a microphone and I'm willing to communicate."
    ]

discordTagInputGroup :: forall slots action.
    Maybe String -> (Maybe String -> action) -> Boolean -> HTML slots action
discordTagInputGroup value onValue error =
    inputGroup $
    [ inputLabel "fab fa-discord" "Discord tag"
    , textLineInput value onValue
    , inputUnderlabel "Example: username#1234"
    ]
    <> inputError error "This does not look like a valid Discord tag."

aboutInputGroup :: forall slots action. String -> (String -> action) -> Boolean -> HTML slots action
aboutInputGroup value onValue error =
    inputGroup $
    [ textInput_ value onValue
    , inputUnderlabel """Yo bruha, write about yourself. What are you like? Just how
        bruh are you? What kind of bruhs are you looking for?"""
    ]
    <>
    inputError error "About text cannot be more than 2000 characters long."
