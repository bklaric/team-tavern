module TeamTavern.Client.Components.SelectDeclarative.MultiSelect where

import Prelude

import Async (Async)
import Async.Aff (affToAsync)
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), contains, toLower, trim)
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Prim.Row (class Cons)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.Window as Window

type InputEntry option =
    { option :: option
    , selected :: Boolean
    }

type Entry option =
    { option :: option
    , selected :: Boolean
    , shown :: Boolean
    }

type Input option =
    { entries :: Array (InputEntry option)
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    , filter :: Maybe { placeholder :: String, text :: String }
    }

type State option =
    { entries :: Array (Entry option)
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    , filter :: Maybe { placeholder :: String, text :: String }
    , open :: Boolean
    , keepOpen :: Boolean
    , windowSubscription :: Maybe (H.SubscriptionId)
    }

data Action option
    = Init
    | Receive (Input option)
    | Finalize
    | ToggleOption option
    | Open
    | Close
    | TryClose
    | KeepOpen
    | FilterInput String

data Output option
    = SelectedChanged (Array (InputEntry option))
    | FilterChanged String

type Slot option = H.Slot (Const Void) (Output option)

render :: forall slots option. State option -> HH.HTML slots (Action option)
render { entries, labeler, comparer, filter, open } =
    HH.div [ HP.class_ $ HH.ClassName "select" ] $
    [ HH.div
        [ HP.class_ $ HH.ClassName
            if open then "selected-open" else "selected-closed"
        , HE.onMouseDown $ const $ Just if open then Close else Open
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
        Just { placeholder, text } -> Array.singleton $
            HH.div [ HP.class_ $ HH.ClassName "select-filter" ] $
            Array.singleton $
            HH.input
                [ HP.class_ $ HH.ClassName "select-filter-input"
                , HP.placeholder placeholder
                , HP.value text
                , HE.onMouseDown $ const $ Just $ KeepOpen
                , HE.onValueInput $ Just <<< FilterInput
                ])
        <>
        [ HH.div
            [ HP.class_ $ HH.ClassName
                if isJust filter
                then "filterable-options"
                else "options"
            , HE.onMouseDown $ const $ Just $ KeepOpen
            ]
            (entries # Array.filter _.shown <#> \{ option, selected } ->
                HH.div
                [ HP.class_ $ HH.ClassName "option"
                , HE.onClick $ const $ Just $ ToggleOption option
                ]
                [ HH.input
                    [ HP.type_ HP.InputCheckbox
                    , HP.checked selected
                    , HP.tabIndex $ -1
                    , HP.class_ $ HH.ClassName "checkbox-input"
                    ]
                , HH.text $ labeler option
                ])
        ]
    else []

handleAction
    :: forall option slots left
    .  (Action option)
    -> H.HalogenM (State option) (Action option) slots
        (Output option) (Async left) Unit
handleAction Init = do
    window <- H.liftEffect $ Window.toEventTarget <$> window
    let windowEventSource = ES.eventListenerEventSource
            (E.EventType "mousedown") window \_ -> Just TryClose
    windowSubscription <- H.subscribe $ ES.hoist affToAsync windowEventSource
    H.modify_ (_ { windowSubscription = Just windowSubscription })
handleAction (Receive { entries, labeler, comparer, filter }) =
    H.modify_ \state -> state
        { entries = entries <#> \{ option, selected } ->
            { option
            , selected
            , shown: case filter of
                Nothing -> true
                Just { text } ->
                    contains
                    (Pattern $ toLower $ trim text)
                    (toLower $ labeler option)
            }
        , labeler = labeler
        , comparer = comparer
        , filter = filter
        }
handleAction Finalize = do
    { windowSubscription } <- H.get
    case windowSubscription of
        Just windowSubscription' -> H.unsubscribe windowSubscription'
        Nothing -> pure unit
handleAction (ToggleOption toggledOption) = do
    nextEntries <- H.gets \state ->
        state.entries <#> \{ option, selected } ->
            if state.comparer option toggledOption
            then { option, selected: not selected }
            else { option, selected }
    H.raise $ SelectedChanged nextEntries
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
handleAction (FilterInput filter) =
    H.raise $ FilterChanged filter

component :: forall option query left.
    H.Component HH.HTML query (Input option) (Output option) (Async left)
component = H.mkComponent
    { initialState: \{ entries, labeler, comparer, filter } ->
        { entries: entries <#> \{ option, selected } ->
            { option, selected, shown: true }
        , labeler
        , comparer
        , filter
        , open: false
        , keepOpen: false
        , windowSubscription: Nothing
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , receive = Just <<< Receive
        , finalize = Just Finalize
        }
    }

multiSelect
    :: forall children' slot children action left option
    .  Cons slot (Slot option Unit) children' children
    => IsSymbol slot
    => SProxy slot
    -> Input option
    -> (Output option -> Maybe action)
    -> HH.HTML (H.ComponentSlot HH.HTML children (Async left) action) action
multiSelect label input handleOutput =
    HH.slot label unit component input handleOutput

multiSelectIndexed
    :: forall children' slot children action left option index
    .  Cons slot (Slot option index) children' children
    => IsSymbol slot
    => Ord index
    => SProxy slot
    -> index
    -> Input option
    -> (Output option -> Maybe action)
    -> HH.HTML (H.ComponentSlot HH.HTML children (Async left) action) action
multiSelectIndexed label index input handleOutput =
    HH.slot label index component input handleOutput
