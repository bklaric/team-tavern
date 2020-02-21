module TeamTavern.Client.Components.MultiSelect where

import Prelude

import Async (Async)
import Async.Aff (affToAsync)
import Control.Monad.State (class MonadState)
import Data.Array as Array
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
import Web.Event.Event (preventDefault)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

type InputOption option =
    { option :: option
    , selected :: Boolean
    }

type Option option =
    { option :: option
    , selected :: Boolean
    , shown :: Boolean
    }

type Input option =
    { options :: Array (InputOption option)
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    , showFilter :: Maybe String
    }

type State option =
    { options :: Array (Option option)
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    , showFilter :: Maybe String
    , open :: Boolean
    , keepOpen :: Boolean
    , windowSubscription :: Maybe (H.SubscriptionId)
    }

data Action option
    = Init
    | ToggleOption option
    | Open
    | Close
    | TryClose
    | KeepOpen
    | PreventDefault MouseEvent
    | FilterInput String
    | Finalize

data Query option send
    = Selected (Array option -> send)
    | Clear send

type Slot option = H.Slot (Query option) Void

render :: forall slots option. State option -> HH.HTML slots (Action option)
render { options, labeler, comparer, showFilter, open } =
    HH.div
    [ HP.class_ $ HH.ClassName "select"
    , HP.tabIndex 0
    ]
    $ [ HH.div
        [ HP.class_ $ HH.ClassName
            if open then "selected-open" else "selected-closed"
        , HE.onMouseDown $ const $ Just if open then Close else Open
        ]
        [ HH.text $ intercalate ", "
            (options # Array.filter (_.selected) <#> (_.option >>> labeler))
        ]
    ]
    <> if open
        then
            (case showFilter of
            Nothing -> []
            Just placeholder ->
                [ HH.input
                    [ HP.class_ $ HH.ClassName "select-filter"
                    , HP.placeholder placeholder
                    , HE.onMouseDown $ const $ Just $ KeepOpen
                    , HE.onValueInput $ Just <<< FilterInput
                    ]
                ])
            <>
            [ HH.div
                [ HP.class_ $ HH.ClassName
                    if isJust showFilter
                    then "filterable-options"
                    else "options"
                , HE.onMouseDown $ const $ Just $ KeepOpen
                ]
                (options # Array.filter _.shown <#> \{ option, selected } ->
                    HH.div
                    [ HP.class_ $ HH.ClassName "option"
                    , HE.onClick $ const $ Just $ ToggleOption option
                    ]
                    [ HH.input
                        [ HP.type_ HP.InputCheckbox
                        , HP.checked selected
                        , HP.tabIndex $ -1
                        , HE.onMouseDown $ Just <<< PreventDefault
                        , HP.class_ $ HH.ClassName "checkbox-input"
                        ]
                    , HH.text $ labeler option
                    ])
            ]
        else
            []

handleAction :: forall option slots message left.
    (Action option) -> H.HalogenM (State option) (Action option) slots message (Async left) Unit
handleAction Init = do
    window <- H.liftEffect $ Window.toEventTarget <$> window
    let windowEventSource = ES.eventListenerEventSource
            (E.EventType "mousedown") window \_ -> Just TryClose
    windowSubscription <- H.subscribe $ ES.hoist affToAsync windowEventSource
    H.modify_ (_ { windowSubscription = Just windowSubscription })
    pure unit
handleAction Finalize = do
    { windowSubscription } <- H.get
    case windowSubscription of
        Just windowSubscription' -> H.unsubscribe windowSubscription'
        Nothing -> pure unit
handleAction (ToggleOption toggledOption) =
    H.modify_ (\state -> state
        { options = state.options <#> \{ option, selected, shown } ->
            if state.comparer option toggledOption
            then { option, selected: not selected, shown }
            else { option, selected, shown }
        })
handleAction Open =
    H.modify_ \state -> state
        { options = state.options <#> (_ { shown = true })
        , open = true
        , keepOpen = true
        }
handleAction Close =
    H.modify_ \state -> state { open = false }
handleAction TryClose =
    H.modify_ \state ->
        if state.keepOpen
        then state { keepOpen = false }
        else state { open = false }
handleAction KeepOpen =
    H.modify_ (_ { keepOpen = true })
handleAction (PreventDefault mouseEvent) =
    H.liftEffect $ preventDefault (MouseEvent.toEvent mouseEvent)
handleAction (FilterInput filter) = do
    H.modify_ \state -> state
        { options = state.options <#> \option -> option
            { shown = contains
                (Pattern $ toLower $ trim filter)
                (toLower $ state.labeler option.option)
            }
        }

handleQuery
    :: forall option monad send
    . Bind monad => MonadState (State option) monad
    => Query option send -> monad (Maybe send)
handleQuery (Selected send) = do
    { options } <- H.get
    pure $ Just $ send (options # Array.filter (_.selected) <#> _.option)
handleQuery (Clear send) = do
    H.modify_ \state @ { options } ->
        state { options = options <#> \option ->
            option { selected = false }
        }
    pure $ Just send

component :: forall option message left.
    H.Component HH.HTML (Query option) (Input option) message (Async left)
component = H.mkComponent
    { initialState: \{ options, labeler, comparer, showFilter } ->
        { options: options <#> \{ option, selected } ->
            { option, selected, shown: true }
        , labeler
        , comparer
        , showFilter
        , open: false
        , keepOpen: false
        , windowSubscription: Nothing
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Init
        , finalize = Just Finalize
        }
    }

multiSelect
    :: forall children' slot children output left option
    .  Cons slot (Slot option Unit) children' children
    => IsSymbol slot
    => SProxy slot
    -> Input option
    -> HH.HTML (H.ComponentSlot HH.HTML children (Async left) output) output
multiSelect label input = HH.slot label unit component input absurd

multiSelectIndexed
    :: forall children' slot children output left option index
    .  Cons slot (Slot option index) children' children
    => IsSymbol slot
    => Ord index
    => SProxy slot
    -> index
    -> Input option
    -> HH.HTML (H.ComponentSlot HH.HTML children (Async left) output) output
multiSelectIndexed label index input =
    HH.slot label index component input absurd
