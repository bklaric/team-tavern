module TeamTavern.Client.Components.Select.MultiSelect
    (Input, Output, Slot, multiSelect, multiSelectIndexed) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String (Pattern(..), contains, toLower, trim)
import Data.Symbol (class IsSymbol)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as ES
import Prim.Row (class Cons)
import TeamTavern.Client.Components.Checkable (checkbox)
import TeamTavern.Client.Components.Input (checkboxLabel)
import TeamTavern.Client.Shared.Slot (Slot_OI)
import Type.Proxy (Proxy)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.Window as Window

type Input option =
    { options :: Array option
    , selected :: Array option
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    , filter :: Maybe String
    }

type Entry option =
    { option :: option
    , selected :: Boolean
    , shown :: Boolean
    }

type State option =
    { entries :: Array (Entry option)
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    , filter :: Maybe { placeHolder :: String, text :: String }
    , open :: Boolean
    , keepOpen :: Boolean
    , windowSubscription :: Maybe (H.SubscriptionId)
    }

data Action option
    = Initialize
    | Receive (Input option)
    | Finalize
    | ToggleOption option
    | Open
    | Close
    | TryClose
    | KeepOpen
    | FilterInput String

type Output option = Array option

type Slot option index = Slot_OI (Output option) index

render :: ∀ slots option. State option -> HH.HTML slots (Action option)
render { entries, labeler, filter, open } =
    HH.div [ HP.class_ $ HH.ClassName "select" ] $
    [ HH.div
        [ HP.class_ $ HH.ClassName
            if open then "selected-open" else "selected-closed"
        , HE.onMouseDown $ const if open then Close else Open
        ]
        [ HH.text $ intercalate ", "
            (entries # Array.filter (_.selected) <#> (_.option >>> labeler))
        ]
    ]
    <>
    if open
    then
        (case filter of
        Nothing -> []
        Just { placeHolder, text } -> Array.singleton $
            HH.div [ HP.class_ $ HH.ClassName "select-filter" ] $
            Array.singleton $
            HH.input
                [ HP.class_ $ HH.ClassName "select-filter-input"
                , HP.placeholder placeHolder
                , HP.value text
                , HE.onMouseDown $ const KeepOpen
                , HE.onValueInput FilterInput
                ])
        <>
        [ HH.div
            [ HP.class_ $ HH.ClassName
                if isJust filter
                then "filterable-options"
                else "options"
            , HE.onMouseDown $ const $ KeepOpen
            ]
            (entries # Array.filter _.shown <#> \{ option, selected } ->
                HH.div
                [ HP.class_ $ HH.ClassName "option"
                , HE.onClick $ const $ ToggleOption option
                ]
                [ checkbox selected
                , checkboxLabel $ labeler option
                ])
        ]
    else []

handleAction
    :: ∀ option slots left
    .  (Action option)
    -> H.HalogenM (State option) (Action option) slots
        (Output option) (Async left) Unit
handleAction Initialize = do
    window <- H.liftEffect $ Window.toEventTarget <$> window
    let windowEventSource = ES.eventListener
            (E.EventType "mousedown") window \_ -> Just TryClose
    windowSubscription <- H.subscribe windowEventSource
    H.modify_ (_ { windowSubscription = Just windowSubscription })
handleAction (Receive input) =
    H.modify_ \state -> state
        { entries = input.options <#> \option ->
            { option
            , selected: Array.any (input.comparer option) input.selected
            , shown:
                case state.filter of
                Just { text } ->
                    contains
                    (Pattern $ toLower $ trim text)
                    (toLower $ input.labeler option)
                Nothing -> true
            }
        , labeler = input.labeler
        , comparer = input.comparer
        , filter = input.filter
            <#> { placeHolder: _, text: maybe "" _.text state.filter }
        }
handleAction Finalize = do
    { windowSubscription } <- H.get
    case windowSubscription of
        Just windowSubscription' -> H.unsubscribe windowSubscription'
        Nothing -> pure unit
handleAction (ToggleOption toggledOption) = do
    { entries } <- H.modify \state -> state
        { entries = state.entries <#> \entry ->
            if state.comparer entry.option toggledOption
            then entry { selected = not entry.selected }
            else entry
        }
    H.raise (entries # Array.filter _.selected <#> _.option)
handleAction Open =
    H.modify_ (_ { open = true, keepOpen = true })
handleAction Close =
    H.modify_ (_ { open = false })
handleAction TryClose =
    H.modify_ \state ->
        if state.keepOpen
        then state { keepOpen = false }
        else state { open = false }
handleAction KeepOpen =
    H.modify_ (_ { keepOpen = true })
handleAction (FilterInput text) =
    H.modify_ \state @ { labeler } -> state
        { entries = state.entries <#> \entry @ { option } ->
            entry
            { shown =
                contains
                (Pattern $ toLower $ trim text)
                (toLower $ labeler option)
            }
        , filter = state.filter <#> (_ { text = text })
        }

component :: ∀ query option left.
    H.Component query (Input option) (Output option) (Async left)
component = H.mkComponent
    { initialState: \{ options, selected, labeler, comparer, filter } ->
        { entries: options <#> \option ->
            { option
            , selected: Array.any (comparer option) selected
            , shown: true
            }
        , labeler
        , comparer
        , filter: filter <#> { placeHolder: _, text: "" }
        , open: false
        , keepOpen: false
        , windowSubscription: Nothing
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        , finalize = Just Finalize
        }
    }

multiSelect
    :: ∀ children' slot children action left option
    .  Cons slot (Slot option Unit) children' children
    => IsSymbol slot
    => Proxy slot
    -> Input option
    -> (Output option -> action)
    -> HH.HTML (H.ComponentSlot children (Async left) action) action
multiSelect label input handleOutput =
    HH.slot label unit component input handleOutput

multiSelectIndexed
    :: ∀ children' slot children action left option index
    .  Cons slot (Slot option index) children' children
    => IsSymbol slot
    => Ord index
    => Proxy slot
    -> index
    -> Input option
    -> (Output option -> action)
    -> HH.HTML (H.ComponentSlot children (Async left) action) action
multiSelectIndexed label index input handleOutput =
    HH.slot label index component input handleOutput
