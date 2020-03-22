module TeamTavern.Client.Components.SelectImperative.MultiSelect where

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

type LoadedState option =
    { options :: Array (Option option)
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
    = ToggleOption option
    | Open
    | Close
    | TryClose
    | KeepOpen
    | PreventDefault MouseEvent
    | FilterInput String
    | Finalize

data Query option send
    = SetOptions (Input option) send
    | GetSelected (Array option -> send)
    | Clear send

type Slot option = H.Slot (Query option) Void Unit

type SlotIndexed option index = H.Slot (Query option) Void index

render :: forall slots option. State option -> HH.HTML slots (Action option)
render Empty = HH.div_ []
render (Loaded { options, labeler, comparer, showFilter, open }) =
    HH.div [ HP.class_ $ HH.ClassName "select" ]
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

handleAction :: forall option slots message left.
    (Action option) -> H.HalogenM (State option) (Action option) slots message (Async left) Unit
handleAction Finalize = do
    state <- H.get
    case state of
        Empty -> pure unit
        Loaded { windowSubscription } -> H.unsubscribe windowSubscription
handleAction (ToggleOption toggledOption) =
    modifyLoaded_ (\state -> state
        { options = state.options <#> \{ option, selected, shown } ->
            if state.comparer option toggledOption
            then { option, selected: not selected, shown }
            else { option, selected, shown }
        })
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
handleAction (PreventDefault mouseEvent) =
    H.liftEffect $ preventDefault (MouseEvent.toEvent mouseEvent)
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
handleQuery (SetOptions { options, labeler, comparer, showFilter } send) = do
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
        { options: options <#> \{ option, selected } ->
            { option, selected, shown: true }
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
        Loaded { options } -> pure $ Just $
            send (options # Array.filter (_.selected) <#> _.option)
handleQuery (Clear send) = do
    modifyLoaded_ \state @ { options } ->
        state { options = options <#> \option ->
            option { selected = false }
        }
    pure $ Just send

component :: forall option input message left.
    H.Component HH.HTML (Query option) input message (Async left)
component = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , finalize = Just Finalize
        }
    }

multiSelect
    :: forall children' slot children output left option
    .  Cons slot (Slot option) children' children
    => IsSymbol slot
    => SProxy slot
    -> HH.HTML (H.ComponentSlot HH.HTML children (Async left) output) output
multiSelect label = HH.slot label unit component unit absurd

multiSelectIndexed
    :: forall children' slot children output left option index
    .  Cons slot (SlotIndexed option index) children' children
    => IsSymbol slot
    => Ord index
    => SProxy slot
    -> index
    -> HH.HTML (H.ComponentSlot HH.HTML children (Async left) output) output
multiSelectIndexed label index =
    HH.slot label index component unit absurd
