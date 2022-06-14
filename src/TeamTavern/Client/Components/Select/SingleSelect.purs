module TeamTavern.Client.Components.Select.SingleSelect
    (Input, Output, Slot, singleSelect, singleSelectIndexed) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (find)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), contains, toLower, trim)
import Data.Symbol (class IsSymbol)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as ES
import Prim.Row (class Cons)
import Type.Proxy (Proxy)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.Window as Window

type Entry option =
    { option :: option
    , shown :: Boolean
    }

type Input option =
    { options :: Array option
    , selected :: Maybe option
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    , filter :: Maybe String
    }

type State option =
    { entries :: Array (Entry option)
    , selected :: Maybe option
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    , filter :: Maybe { placeHolder :: String, text :: String }
    , open :: Boolean
    , keepOpen :: Boolean
    , windowSubscription :: Maybe H.SubscriptionId
    }

data Action option
    = Initialize
    | Receive (Input option)
    | Finalize
    | Select (Maybe option)
    | Open
    | Close
    | TryClose
    | KeepOpen
    | FilterInput String

type Output option = Maybe option

type Slot option = H.Slot (Const Void) (Output option)

render :: forall slots option. State option -> HH.HTML slots (Action option)
render { entries, selected, labeler, filter, open } =
    HH.div [ HP.class_ $ HH.ClassName "select" ]
    $ [ HH.div
        [ HP.class_ $ HH.ClassName
            if open then "selected-open" else "selected-closed"
        , HE.onMouseDown $ const if open then Close else Open
        ]
        [ HH.text
            case selected of
            Nothing -> ""
            Just option -> labeler option
        ]
    ]
    <> if open
        then
            (case filter of
            Nothing -> []
            Just { placeHolder, text } ->
                [ HH.div [ HP.class_ $ HH.ClassName "select-filter" ] $
                Array.singleton $
                HH.input
                    [ HP.class_ $ HH.ClassName "select-filter-input"
                    , HP.placeholder placeHolder
                    , HP.value text
                    , HE.onMouseDown $ const KeepOpen
                    , HE.onValueInput FilterInput
                    ]
                ])
            <>
            [ HH.div
                [ HP.class_ $ HH.ClassName
                    if isJust filter
                    then "filterable-options"
                    else "options"
                , HE.onMouseDown $ const KeepOpen
                ] $
                [ HH.div
                    [ HP.class_ $ HH.ClassName "option"
                    , HE.onClick $ const $ Select Nothing
                    ]
                    [ HH.text "" ]
                ]
                <>
                (entries # Array.filter _.shown <#> \option ->
                    HH.div
                    [ HP.class_ $ HH.ClassName "option"
                    , HE.onClick $ const $ Select $ Just $ option.option
                    ]
                    [ HH.text $ labeler option.option ])
            ]
        else []

handleAction
    :: forall option slots left
    .  (Action option)
    -> H.HalogenM (State option) (Action option) slots
        (Output option) (Async left) Unit
handleAction Initialize = do
    window <- H.liftEffect $ Window.toEventTarget <$> window
    let windowEventSource = ES.eventListener
            (E.EventType "mousedown") window \_ -> Just TryClose
    windowSubscription <- H.subscribe windowEventSource
    H.modify_ (_ { windowSubscription = Just windowSubscription })
handleAction (Receive { selected }) =
    H.modify_ \state -> state
        { selected = selected >>= \selected' ->
            state.entries <#> _.option # find (state.comparer selected')
        }
handleAction Finalize = do
    { windowSubscription } <- H.get
    case windowSubscription of
        Just windowSubscription' -> H.unsubscribe windowSubscription'
        Nothing -> pure unit
handleAction (Select selected) = do
    H.modify_ (_ { selected = selected, open = false })
    H.raise selected
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

component :: forall option query left.
    H.Component query (Input option) (Output option) (Async left)
component = H.mkComponent
    { initialState: \{ options, selected, labeler, comparer, filter } ->
        { entries: options <#> { option: _, shown: true }
        , selected: selected >>= \selected' ->
            options # find (\option -> comparer option selected')
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

singleSelect
    :: forall children' slot children action left option
    .  Cons slot (Slot option Unit) children' children
    => IsSymbol slot
    => Proxy slot
    -> Input option
    -> (Output option -> action)
    -> HH.HTML (H.ComponentSlot children (Async left) action) action
singleSelect label input handleOutput =
    HH.slot label unit component input handleOutput

singleSelectIndexed
    :: forall children' slot children action left option index
    .  Cons slot (Slot option index) children' children
    => IsSymbol slot
    => Ord index
    => Proxy slot
    -> index
    -> Input option
    -> (Output option -> action)
    -> HH.HTML (H.ComponentSlot children (Async left) action) action
singleSelectIndexed label index input handleOutput =
    HH.slot label index component input handleOutput
