module TeamTavern.Client.Components.Player.ProfileInputGroup where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Foldable (find)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Variant (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Input (checkboxInput, inputError, inputGroup, inputLabel, inputLabel', inputUnderlabel, textInput_, textLineInput)
import TeamTavern.Client.Components.Select.MultiSelect (multiSelectIndexed)
import TeamTavern.Client.Components.Select.MultiSelect as MultiSelect
import TeamTavern.Client.Components.Select.SingleSelect (singleSelectIndexed)
import TeamTavern.Client.Components.Select.SingleSelect as SingleSelect

type Option =
    { key :: String
    , label :: String
    }

type Field =
    { key :: String
    , ilk :: Int
    , label :: String
    , icon :: String
    , required :: Boolean
    , domain :: Maybe String
    , options :: Maybe (Array Option)
    }

type FieldValue =
    { fieldKey :: String
    , url :: Maybe String
    , optionKey :: Maybe String
    , optionKeys :: Maybe (Array String)
    }

type FieldValues = Map String FieldValue

type ChildSlots =
    ( "singleSelectField" :: SingleSelect.Slot Option String
    , "multiSelectField" :: MultiSelect.Slot Option String
    )

fieldInputGroup
    :: forall action left
    .  FieldValues
    -> (String -> Maybe String -> action)
    -> (String -> Maybe String -> action)
    -> (String -> Array String -> action)
    -> Array String
    -> Array String
    -> Field
    -> H.ComponentHTML action ChildSlots (Async left)
fieldInputGroup fieldValues onValue _ _ urlValueErrors missingErrors
    { ilk: 1, key, label, icon, required, domain: Just domain } =
    let
    fieldValue' = Map.lookup key fieldValues
    url = fieldValue' >>= _.url
    urlError = urlValueErrors # Array.any (_ == key)
    missingError = missingErrors # Array.any (_ == key)
    in
    inputGroup $
    [ inputLabel' icon label (Just domain) required
    , textLineInput url (onValue key)
    ]
    <> inputError urlError
        ("This doesn't look like a valid " <> label <> " (" <> domain <> ") address.")
    <> inputError missingError (label <> " is required.")
fieldInputGroup fieldValues _ onValue _ _ _
    { ilk: 2, key, label, icon, options: Just options } =
    let
    fieldValue' = Map.lookup key fieldValues
    selected = fieldValue' >>= _.optionKey >>= \optionKey ->
        options # find \option -> optionKey == option.key
    in
    inputGroup
    [ inputLabel icon label
    , singleSelectIndexed (SProxy :: SProxy "singleSelectField") key
        { options
        , selected
        , labeler: _.label
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        , filter: Nothing
        }
        \option -> Just $ onValue key (option <#> _.key)
    ]
fieldInputGroup fieldValues _ _ onValue _ _
    { ilk: 3, key, label, icon, options: Just options } =
    let
    fieldValue' = fieldValues # find \{ fieldKey } -> fieldKey == key
    selected = fieldValue' >>= _.optionKeys # maybe [] \selectedOptionKeys ->
        options # Array.filter \option -> Array.any (_ == option.key) selectedOptionKeys
    in
    inputGroup
    [ inputLabel icon label
    , multiSelectIndexed (SProxy :: SProxy "multiSelectField") key
        { options
        , selected
        , labeler: _.label
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        , filter: Nothing
        }
        \options' -> Just $ onValue key (options' <#> _.key)
    ]
fieldInputGroup _ _ _ _ _ _ _ = HH.div_ []

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
