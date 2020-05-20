module TeamTavern.Client.Components.SelectImperative.SingleSelect where

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

type LoadedState option =
    { options :: Array (Option option)
    , selected :: Maybe option
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    , showFilter :: Maybe String
    , open :: Boolean
    , keepOpen :: Boolean
    , windowSubscription :: H.SubscriptionId
    }

data State option
    = Empty
    | Loaded (LoadedState option)

data Action option
    = Select (Maybe option)
    | Open
    | Close
    | TryClose
    | KeepOpen
    | FilterInput String
    | Finalize

data Query option send
    = SetOptions (Input option) send
    | GetSelected (Maybe option -> send)

data Message option = SelectedChanged (Maybe option)

type Slot option = H.Slot (Query option) (Message option) Unit

type SlotIndexed option index = H.Slot (Query option) (Message option) index

render :: forall slots option. State option -> HH.HTML slots (Action option)
render Empty = HH.div_ []
render (Loaded { options, selected, labeler, showFilter, open }) =
    HH.div [ HP.class_ $ HH.ClassName "select" ]
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
                [ HH.div [ HP.class_ $ HH.ClassName "select-filter" ] $ pure $
                HH.input
                    [ HP.class_ $ HH.ClassName "select-filter-input"
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

modifyLoaded_
    :: forall monad option
    .  MonadState (State option) monad
    => (LoadedState option -> LoadedState option)
    -> monad Unit
modifyLoaded_ function =
    H.modify_
        case _ of
        Empty -> Empty
        Loaded state -> Loaded $ function state

handleAction :: forall option slots left.
    (Action option) -> H.HalogenM (State option) (Action option) slots (Message option) (Async left) Unit
handleAction Finalize = do
    state <- H.get
    case state of
        Empty -> pure unit
        Loaded { windowSubscription } -> H.unsubscribe windowSubscription
handleAction (Select selected) = do
    modifyLoaded_ (_ { selected = selected, open = false })
    H.raise $ SelectedChanged selected
handleAction Open =
    modifyLoaded_ \state -> state
        { options = state.options <#> (_ { shown = true })
        , open = true
        , keepOpen = true
        }
handleAction Close =
    modifyLoaded_ \state -> state { open = false }
handleAction TryClose =
    modifyLoaded_ \state ->
        if state.keepOpen
        then state { keepOpen = false }
        else state { open = false }
handleAction KeepOpen =
    modifyLoaded_ (_ { keepOpen = true })
handleAction (FilterInput filter) = do
    modifyLoaded_ \state -> state
        { options = state.options <#> \option -> option
            { shown = contains
                (Pattern $ toLower $ trim filter)
                (toLower $ state.labeler option.option)
            }
        }

handleQuery
    :: forall message slots left option send
    .  Query option send
    -> H.HalogenM (State option) (Action option) slots message (Async left) (Maybe send)
handleQuery (SetOptions { options, selected, labeler, comparer, showFilter } send) = do
    state <- H.get
    windowSubscription <-
        case state of
        Empty -> do
            window <- H.liftEffect $ Window.toEventTarget <$> window
            let windowEventSource = ES.eventListenerEventSource
                    (E.EventType "mousedown") window \_ -> Just TryClose
            windowSubscription <- H.subscribe $
                ES.hoist affToAsync windowEventSource
            pure windowSubscription
        Loaded { windowSubscription } -> pure windowSubscription
    H.put $ Loaded
        { options: options <#> { option: _, shown: true }
        , selected: selected >>= \selected' ->
            options # find (\option -> comparer option selected')
        , labeler
        , comparer
        , showFilter
        , open: false
        , keepOpen: false
        , windowSubscription
        }
    pure $ Just send
handleQuery (GetSelected send) = do
    state <- H.get
    case state of
        Empty -> pure Nothing
        Loaded { selected } -> pure $ Just $ send selected

component :: forall option input left.
    H.Component HH.HTML (Query option) input (Message option) (Async left)
component = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , finalize = Just Finalize
        }
    }

singleSelect
    :: forall children' slot children action left option
    .  Cons slot (Slot option) children' children
    => IsSymbol slot
    => SProxy slot
    -> HH.HTML (H.ComponentSlot HH.HTML children (Async left) action) action
singleSelect label = HH.slot label unit component unit (const Nothing)

singleSelect'
    :: forall children' slot children action left option
    .  Cons slot (Slot option) children' children
    => IsSymbol slot
    => SProxy slot
    -> (Message option -> Maybe action)
    -> HH.HTML (H.ComponentSlot HH.HTML children (Async left) action) action
singleSelect' label handleMessage =
    HH.slot label unit component unit handleMessage

singleSelectIndexed
    :: forall children' slot children action left option index
    .  Cons slot (SlotIndexed option index) children' children
    => IsSymbol slot
    => Ord index
    => SProxy slot
    -> index
    -> HH.HTML (H.ComponentSlot HH.HTML children (Async left) action) action
singleSelectIndexed label index =
    HH.slot label index component unit (const Nothing)
