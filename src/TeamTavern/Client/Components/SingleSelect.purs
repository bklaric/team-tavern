module TeamTavern.Client.Components.SingleSelect where

import Prelude

import Async (Async)
import Async.Aff (affToAsync)
import Control.Monad.State (class MonadState)
import Data.Array as Array
import Data.Foldable (find)
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

type Option option =
    { option :: option
    , shown :: Boolean
    }

type Input option =
    { options :: Array option
    , selected :: Maybe option
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    , showFilter :: Maybe String
    }

type State option =
    { options :: Array (Option option)
    , selected :: Maybe option
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    , showFilter :: Maybe String
    , open :: Boolean
    , keepOpen :: Boolean
    , windowSubscription :: Maybe (H.SubscriptionId)
    }

data Action option
    = Init
    | Select (Maybe option)
    | Open
    | Close
    | TryClose
    | KeepOpen
    | FilterInput String
    | Finalize

data Query option send = Selected (Maybe option -> send)

data Message option = SelectedChanged (Maybe option)

type Slot option = H.Slot (Query option) (Message option)

render :: forall slots option. State option -> HH.HTML slots (Action option)
render { options, selected, labeler, showFilter, open } =
    HH.div
    [ HP.class_ $ HH.ClassName "select"
    , HP.tabIndex 0
    ]
    $ [ HH.div
        [ HP.class_ $ HH.ClassName
            if open then "selected-open" else "selected-closed"
        , HE.onMouseDown $ const $ Just if open then Close else Open
        ]
        [ HH.text
            case selected of
            Nothing -> ""
            Just option -> labeler option
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
                ] $
                [ HH.div
                    [ HP.class_ $ HH.ClassName "option"
                    , HE.onClick $ const $ Just $ Select Nothing
                    ]
                    [ HH.text "" ]
                ]
                <>
                (options # Array.filter _.shown <#> \option ->
                    HH.div
                    [ HP.class_ $ HH.ClassName "option"
                    , HE.onClick $ const $ Just $ Select $ Just $ option.option
                    ]
                    [ HH.text $ labeler option.option ])
            ]
        else
            []

handleAction :: forall option slots left.
    (Action option) -> H.HalogenM (State option) (Action option) slots (Message option) (Async left) Unit
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
handleAction (Select selected) = do
    H.modify_ (_ { selected = selected, open = false })
    H.raise $ SelectedChanged selected
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
        else state { keepOpen = false, open = false }
handleAction KeepOpen =
    H.modify_ (_ { keepOpen = true })
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
    { selected } <- H.get
    pure $ Just $ send selected

component :: forall option left.
    H.Component HH.HTML (Query option) (Input option) (Message option) (Async left)
component = H.mkComponent
    { initialState: \{ options, selected, labeler, comparer, showFilter } ->
        { options: options <#> { option: _, shown: true }
        , selected: selected >>= \selected' ->
            options # find (\option -> comparer option selected')
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

singleSelect
    :: forall children' slot children action left option
    .  Cons slot (Slot option Unit) children' children
    => IsSymbol slot
    => SProxy slot
    -> Input option
    -> HH.HTML (H.ComponentSlot HH.HTML children (Async left) action) action
singleSelect label input = HH.slot label unit component input (const Nothing)

singleSelect'
    :: forall children' slot children action left option
    .  Cons slot (Slot option Unit) children' children
    => IsSymbol slot
    => SProxy slot
    -> Input option
    -> (Message option -> Maybe action)
    -> HH.HTML (H.ComponentSlot HH.HTML children (Async left) action) action
singleSelect' label input handleMessage =
    HH.slot label unit component input handleMessage

singleSelectIndexed
    :: forall children' slot children action left option index
    .  Cons slot (Slot option index) children' children
    => IsSymbol slot
    => Ord index
    => SProxy slot
    -> index
    -> Input option
    -> HH.HTML (H.ComponentSlot HH.HTML children (Async left) action) action
singleSelectIndexed label index input =
    HH.slot label index component input (const Nothing)
