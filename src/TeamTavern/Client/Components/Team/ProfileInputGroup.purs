module TeamTavern.Client.Components.Team.ProfileInputGroup where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Maybe (Maybe(..), maybe)
import Data.MultiMap (MultiMap)
import Data.MultiMap as MultiMap
import Data.Variant (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Input (checkboxInput, inputError, inputGroup, inputLabel, inputUnderlabel, textInput_)
import TeamTavern.Client.Components.Select.MultiSelect (multiSelectIndexed)
import TeamTavern.Client.Components.Select.MultiSelect as MultiSelect

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

type FieldValues = MultiMap String String

fieldInputGroup
    :: forall action slots left
    .  FieldValues
    -> (String -> (MultiSelect.Output Option) -> action)
    -> Field
    -> H.ComponentHTML
        action
        ("multiSelectField" :: MultiSelect.Slot Option String | slots)
        (Async left)
fieldInputGroup fieldValues onValue field
    =
    inputGroup
    [ inputLabel field.icon field.label
    , multiSelectIndexed (SProxy :: SProxy "multiSelectField") field.key
        { options: field.options
        , selected: MultiMap.lookup field.key fieldValues # maybe [] \optionKeys ->
            field.options # Array.filter \option ->
                optionKeys # Array.any (_ == option.key)
        , labeler: _.label
        , comparer: \optionLeft optionRight -> optionLeft.key == optionRight.key
        , filter: Nothing
        }
        (Just <<< onValue field.key)
    ]

newOrReturningInputGroup :: forall slots action.
    Boolean -> (Boolean -> action) -> HH.HTML slots action
newOrReturningInputGroup value onValue =
    inputGroup
    [ inputLabel "fas fa-book" "Microphone"
    , checkboxInput value onValue "Must be new or returning players to the game."
    ]

ambitionsInputGroup :: forall slots action.
    String -> (String -> action) -> Boolean -> HH.HTML slots action
ambitionsInputGroup value onValue error =
    inputGroup $
    [ textInput_ value onValue
    , inputUnderlabel "Holy shit, why are you even playing this stupid game?"
    ]
    <>
    inputError error "Ambitions text cannot be more than 2000 characters long."
