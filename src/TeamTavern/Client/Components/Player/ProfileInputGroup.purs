module TeamTavern.Client.Components.Player.ProfileInputGroup where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Foldable (find)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Anchor (trackedTextAnchor)
import TeamTavern.Client.Components.Input (aboutInputSubcontent, ambitionsInputSubcontent, checkboxInput, inputError, inputGroup, inputLabel, inputUnderlabel, inputUnderlabel', platformIdLabel, textInput_, textLineInput)
import TeamTavern.Client.Components.Select.MultiSelect (multiSelectIndexed)
import TeamTavern.Client.Components.Select.MultiSelect as MultiSelect
import TeamTavern.Client.Components.Select.SingleSelect (singleSelectIndexed)
import TeamTavern.Client.Components.Select.SingleSelect as SingleSelect
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Brands (inputBattleNetSvg, inputOriginSvg, inputPlayStationSvg, inputRiotSvg, inputSteamSvg, inputSwitchSvg, inputUbisoftSvg, inputXboxSvg)
import TeamTavern.Routes.Shared.Field (Field, Option, ValueSimple)
import TeamTavern.Routes.Shared.Platform (Platform(..))
import Type.Proxy (Proxy(..))

platformIdInputGroup :: forall action other left.
    Platform -> Maybe String -> (Maybe String -> action) -> Boolean -> Boolean ->
    H.ComponentHTML action
        ( blizzardAccountLink :: Slot___
        , riotAccountLink :: Slot___
        , steamAccountLink :: Slot___
        | other
        )
        (Async left)
platformIdInputGroup platform platformId onValue error required =
    inputGroup $
    ( case platform of
        Steam -> [ platformIdLabel inputSteamSvg "Steam ID" required ]
        Riot -> [ platformIdLabel inputRiotSvg "Riot ID" required ]
        BattleNet -> [ platformIdLabel inputBattleNetSvg "BattleTag" required ]
        Origin -> [ platformIdLabel inputOriginSvg "EA ID" required ]
        Ubisoft -> [ platformIdLabel inputUbisoftSvg "Ubisoft Connect username" required ]
        PlayStation -> [ platformIdLabel inputPlayStationSvg "PSN ID" required ]
        Xbox -> [ platformIdLabel inputXboxSvg "Gamertag" required ]
        Switch -> [ platformIdLabel inputSwitchSvg "Friend code" required ]
    )
    <>
    [ textLineInput platformId onValue ]
    <>
    ( case platform of
        Steam ->
            [ inputUnderlabel "Example: 76561198821728791"
            , inputUnderlabel'
                [ HH.text "You can find out your Steam ID at "
                , HH.slot_ (Proxy :: _ "steamAccountLink") unit trackedTextAnchor {url: "https://store.steampowered.com/account", text: "store.steampowered.com/account"}
                ]
            ]
        Riot ->
            [ inputUnderlabel "Example: username#12345"
            , inputUnderlabel'
                [ HH.text "You can find out your Riot ID at "
                , HH.slot_ (Proxy :: _ "riotAccountLink") unit trackedTextAnchor {url: "https://account.riotgames.com/", text: "account.riotgames.com"}
                ]
            , inputUnderlabel "Make sure to include the tagline as well."
            ]
        BattleNet ->
            [ inputUnderlabel "Example: username#1234"
            , inputUnderlabel'
                [ HH.text "You can find out your BattleTag at "
                , HH.slot_ (Proxy :: _ "blizzardAccountLink") unit trackedTextAnchor {url: "https://account.blizzard.com/details", text: "account.blizzard.com"}
                ]
            ]
        Origin -> []
        Ubisoft -> []
        PlayStation -> []
        Xbox -> [ inputUnderlabel "Make sure to include your suffix (e.g. #123) if you have one."]
        Switch -> [ inputUnderlabel "Example: SW-7417-3522-1808" ]
    )
    <>
    inputError error
        case platform of
        Steam -> "This doesn't look like a valid Steam ID."
        Riot -> "This doesn't look like a valid Riot ID."
        BattleNet -> "This doesn't look like a valid BattleTag."
        Origin -> "This doesn't look like a valid EA ID."
        Ubisoft -> "This doesn't look like a valid Ubisoft Connect username."
        PlayStation -> "This doesn't look like a valid PSN ID."
        Xbox -> "This doesn't look like a valid Gamertag."
        Switch -> "This doesn't look like a valid friend code."

type FieldValues = Map String ValueSimple

type ChildSlots =
    ( "singleSelectField" :: SingleSelect.Slot Option String
    , "multiSelectField" :: MultiSelect.Slot Option String
    , steamAccountLink :: Slot___
    , riotAccountLink :: Slot___
    , blizzardAccountLink :: Slot___
    )

fieldInputGroup
    :: ∀ action left
    .  FieldValues
    -> (String -> Maybe String -> action)
    -> (String -> Array String -> action)
    -> Field
    -> H.ComponentHTML action ChildSlots (Async left)
fieldInputGroup fieldValues onValue _
    { ilk: "single", key, label, icon, options } =
    let
    fieldValue' = Map.lookup key fieldValues
    selected = fieldValue' >>= _.optionKey >>= \optionKey ->
        options # find \option -> optionKey == option.key
    in
    inputGroup
    [ inputLabel icon label
    , singleSelectIndexed (Proxy :: _ "singleSelectField") key
        { options
        , selected
        , labeler: _.label
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        , filter: Nothing
        }
        \option -> onValue key (option <#> _.key)
    ]
fieldInputGroup fieldValues _ onValue
    { ilk: "multi", key, label, icon, options } =
    let
    fieldValue' = fieldValues # find \{ fieldKey } -> fieldKey == key
    selected = fieldValue' >>= _.optionKeys # maybe [] \selectedOptionKeys ->
        options # Array.filter \option -> Array.any (_ == option.key) selectedOptionKeys
    in
    inputGroup
    [ inputLabel icon label
    , multiSelectIndexed (Proxy :: _ "multiSelectField") key
        { options
        , selected
        , labeler: _.label
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        , filter: Nothing
        }
        \options' -> onValue key (options' <#> _.key)
    ]
fieldInputGroup _ _ _ _ = HH.div_ []

newOrReturningInputGroup :: ∀ slots action.
    Boolean -> (Boolean -> action) -> HH.HTML slots action
newOrReturningInputGroup value onValue =
    inputGroup
    [ inputLabel "fas fa-book" "New or returning"
    , checkboxInput value onValue "Is a new or returning player to the game."
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
