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
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Input (checkboxInput, domainInputLabel, externalIdLabel, inputError, inputGroup, inputLabel, inputUnderlabel, inputUnderlabel', requiredTextLineInput, textInput_, textLineInput)
import TeamTavern.Client.Components.Select.MultiSelect (multiSelectIndexed)
import TeamTavern.Client.Components.Select.MultiSelect as MultiSelect
import TeamTavern.Client.Components.Select.SingleSelect (singleSelectIndexed)
import TeamTavern.Client.Components.Select.SingleSelect as SingleSelect
import TeamTavern.Client.Snippets.Brands (inputBattleNetSvg, inputRiotSvg, inputSteamSvg)

externalIdInputGroup :: forall slots action.
    Int -> String -> (String -> action) -> Boolean -> HH.HTML slots action
externalIdInputGroup externalIdIlk externalId onValue error =
    inputGroup $
    ( case externalIdIlk of
        1 -> [ externalIdLabel inputSteamSvg "Steam profile" (Just "steamcommunity.com") ]
        2 -> [ externalIdLabel inputRiotSvg "Riot ID" Nothing ]
        3 -> [ externalIdLabel inputBattleNetSvg "BattleTag" Nothing ]
        _ -> []
    )
    <>
    [ requiredTextLineInput externalId onValue ]
    <>
    ( case externalIdIlk of
        1 ->
            [ inputUnderlabel "Example: steamcommunity.com/id/username"
            , inputUnderlabel "Example: steamcommunity.com/profile/76561198821728791"
            ]
        2 ->
            [ inputUnderlabel "Example: username#12345"
            , inputUnderlabel'
                [ HH.text "You can find out your Riot ID at "
                , HH.a [ HP.href "https://account.riotgames.com/", HP.target "_blank" ] [ HH.text "account.riotgames.com" ]
                ]
            ]
        3 ->
            [ inputUnderlabel "Example: username#1234"
            , inputUnderlabel'
                [ HH.text "You can find out your BattleTag at "
                , HH.a [ HP.href "https://account.blizzard.com/details", HP.target "_blank" ] [ HH.text "account.blizzard.com" ]
                ]
            ]
        _ -> []
    )
    <>
    inputError error
        case externalIdIlk of
        1 -> "This doesn't look like a valid Steam profile URL."
        2 -> "This doesn't look like a valid Riot ID."
        3 -> "This doesn't look like a valid BattleTag."
        _ -> "This doesn't look like a valid external ID."

type Option =
    { key :: String
    , label :: String
    }

type Field =
    { key :: String
    , ilk :: Int
    , label :: String
    , icon :: String
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
    -> Field
    -> H.ComponentHTML action ChildSlots (Async left)
fieldInputGroup fieldValues onValue _ _ urlValueErrors
    { ilk: 1, key, label, icon, domain: Just domain } =
    let
    fieldValue' = Map.lookup key fieldValues
    url = fieldValue' >>= _.url
    urlError = urlValueErrors # Array.any (_ == key)
    in
    inputGroup $
    [ domainInputLabel icon label domain
    , textLineInput url (onValue key)
    ]
    <> inputError urlError
        ("This doesn't look like a valid " <> label <> " (" <> domain <> ") address.")
fieldInputGroup fieldValues _ onValue _ _
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
fieldInputGroup fieldValues _ _ onValue _
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
fieldInputGroup _ _ _ _ _ _ = HH.div_ []

newOrReturningInputGroup :: forall slots action.
    Boolean -> (Boolean -> action) -> HH.HTML slots action
newOrReturningInputGroup value onValue =
    inputGroup
    [ inputLabel "fas fa-book" "New or returning"
    , checkboxInput value onValue "Is a new or returning player to the game."
    ]

ambitionsInputGroup :: forall slots action.
    String -> (String -> action) -> Boolean -> HH.HTML slots action
ambitionsInputGroup value onValue error =
    inputGroup $
    [ textInput_ value onValue
    , inputUnderlabel """What do you want to get out of playing in a team?
        Any specific goals you want to achieve?"""
    ]
    <>
    inputError error "Ambitions text cannot be more than 2000 characters long."
