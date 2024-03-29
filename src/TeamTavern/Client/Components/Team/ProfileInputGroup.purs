module TeamTavern.Client.Components.Team.ProfileInputGroup where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Foldable (any)
import Data.Maybe (Maybe(..), maybe)
import Data.MultiMap (MultiMap)
import Data.MultiMap as MultiMap
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Input (aboutInputSubcontent, ambitionsInputSubcontent, checkboxInput, inputGroup, inputLabel, textInput_)
import TeamTavern.Client.Components.Select.MultiSelect (multiSelectIndexed)
import TeamTavern.Client.Components.Select.MultiSelect as MultiSelect
import TeamTavern.Routes.Shared.Field (Field, Option)
import Type.Proxy (Proxy(..))

type FieldValues = MultiMap String String

fieldInputGroup
    :: ∀ action slots left
    .  FieldValues
    -> (String -> (MultiSelect.Output Option) -> action)
    -> Field
    -> H.ComponentHTML
        action
        (multiSelectField :: MultiSelect.Slot Option String | slots)
        (Async left)
fieldInputGroup fieldValues onValue field =
    inputGroup
    [ inputLabel field.icon field.label
    , multiSelectIndexed (Proxy :: _ "multiSelectField") field.key
        { options: field.options
        , selected: MultiMap.lookup field.key fieldValues # maybe [] \optionKeys ->
            field.options # Array.filter \option ->
                optionKeys # any (_ == option.key)
        , labeler: _.label
        , comparer: \optionLeft optionRight -> optionLeft.key == optionRight.key
        , filter: Nothing
        }
        (onValue field.key)
    ]

newOrReturningInputGroup :: ∀ slots action.
    Boolean -> (Boolean -> action) -> HH.HTML slots action
newOrReturningInputGroup value onValue =
    inputGroup
    [ inputLabel "fas fa-book" "New or returning"
    , checkboxInput value onValue "Must be new or returning players to the game."
    ]

aboutInputGroup :: ∀ slots action.
    String -> (String -> action) -> Boolean -> HH.HTML slots action
aboutInputGroup value onValue error =
    inputGroup $
    [ textInput_ value onValue ]
    <>
    aboutInputSubcontent error value

ambitionsInputGroup :: ∀ slots action.
    String -> (String -> action) -> Boolean -> HH.HTML slots action
ambitionsInputGroup value onValue error =
    inputGroup $
    [ textInput_ value onValue ]
    <>
    ambitionsInputSubcontent error value
