module TeamTavern.Client.Components.SelectDeclarative.SingleTreeSelect
   (Labeler, Comparer, InputEntry(..), Input, Query(..), Output(..), Slot, singleTreeSelect) where

import Prelude

import Async (Async)
import Async.Aff (affToAsync)
import Data.Array as Array
import Data.Foldable (any)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), contains, toLower, trim)
import Data.String as String
import Data.String.Utils (repeat)
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Halogen.Query.HalogenM (SubscriptionId)
import Prim.Row (class Cons)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.Window as Window

newtype InputEntry option = InputEntry
    { option :: option
    , subEntries :: Array (InputEntry option)
    }

newtype Entry option = Entry
    { option :: option
    , shown :: Boolean
    , expanded :: Boolean
    , subEntries :: Array (Entry option)
    }

type Entries option = Array (Entry option)

type Labeler option = option -> String

type Comparer option = option -> option -> Boolean

type Input option =
    { entries :: Array (InputEntry option)
    , selected :: Maybe option
    , labeler :: Labeler option
    , comparer :: Comparer option
    , filter :: String
    }

type State option =
    { entries :: Entries option
    , selected :: Maybe option
    , labeler :: Labeler option
    , comparer :: Comparer option
    , filter :: { placeHolder :: String, text :: String }
    , open :: Boolean
    , keepOpen :: Boolean
    , windowSubscription :: Maybe SubscriptionId
    }

data Action option
    = Initialize
    | Finalize
    | Open
    | Close
    | KeepOpen
    | TryClose
    | Filter String
    | SelectEntry (Maybe option)
    | ToggleEntryExpanded option

data Query option send = Clear send

data Output option = SelectedChanged (Maybe option)

type Slot option = H.Slot (Query option) (Output option) Unit

renderEntry
    :: forall option slots
    .  Int
    -> Labeler option
    -> Entry option
    -> Maybe (Array (HH.HTML slots (Action option)))
renderEntry level labeler (Entry { shown: false }) = Nothing
renderEntry level labeler (Entry { option, expanded, subEntries }) = let
    hasSubEntries = not $ Array.null subEntries
    optionClass =
            (if hasSubEntries then "collapsible-" else "")
            <> (maybe "" identity $ repeat level "sub-") <> "option"
    in
    Just $
    [ HH.div
        [ HP.class_ $ HH.ClassName optionClass
        , HE.onClick $ const $ Just $ SelectEntry $ Just option
        ]
        [ HH.text $ labeler option ]
    ]
    <> Array.catMaybes
    [ if hasSubEntries
        then Just $
            HH.i
            [ HP.class_ $ HH.ClassName $ "fas "
                <> (if expanded then "fa-caret-up" else "fa-caret-down")
                <> " collapsible-option-caret"
            , HE.onMouseDown $ const $ Just $ ToggleEntryExpanded option
            ]
            []
        else Nothing
    ]
    <>
    if expanded
    then (renderEntries (level + 1) labeler subEntries)
    else []

renderEntries
    :: forall option slots
    .  Int
    -> Labeler option
    -> Entries option
    -> Array (HH.HTML slots (Action option))
renderEntries level labeler entries =
    entries <#> renderEntry level labeler # Array.catMaybes # join

render :: forall option slots. State option -> HH.HTML slots (Action option)
render { entries, selected, labeler, comparer, filter, open } = let
    selectedClass = if open then "selected-open" else "selected-closed"
    openOrClose = if open then Close else Open
    in
    HH.div [ HP.class_ $ HH.ClassName "select" ] $
    [ HH.div
        [ HP.class_ $ HH.ClassName selectedClass
        , HE.onMouseDown $ const $ Just openOrClose
        ]
        [ HH.text $ maybe "" labeler selected ]
    ]
    <>
    if open
    then
        [ HH.div [ HP.class_ $ HH.ClassName "select-filter" ] $ pure $
            HH.input
            [ HP.class_ $ HH.ClassName "select-filter-input"
            , HP.placeholder filter.placeHolder
            , HP.value filter.text
            , HE.onMouseDown $ const $ Just $ KeepOpen
            , HE.onValueInput $ Just <<< Filter
            ]
        , HH.div
            [ HP.class_ $ HH.ClassName "filterable-options"
            , HE.onMouseDown $ const $ Just KeepOpen
            ] $
            [ HH.div
                [ HP.class_ $ HH.ClassName "option"
                , HE.onClick $ const $ Just $ SelectEntry Nothing
                ]
                [ HH.text "" ]
            ]
            <>
            (renderEntries 0 labeler entries)
        ]
    else []

filterEntries :: forall option.
    String -> Labeler option -> Entries option -> Entries option
filterEntries filter _ entries | String.null $ trim filter =
    entries <#> \(Entry entry) -> Entry entry { shown = true, expanded = false }
filterEntries filter labeler entries = entries <#> \(Entry entry) -> let
    subEntries = filterEntries filter labeler entry.subEntries
    filterMatches = contains
            (Pattern $ toLower $ trim filter)
            (toLower $ labeler entry.option)
    anySubEntryShown = subEntries # any \(Entry subEntry) -> subEntry.shown
    in
    Entry entry
        { shown = filterMatches || anySubEntryShown
        , expanded = anySubEntryShown
        , subEntries = subEntries
        }

toggleEntriesExpanded :: forall option.
    Comparer option -> option -> Entries option -> Entries option
toggleEntriesExpanded comparer option entries = entries <#> \(Entry entry) ->
    if comparer option entry.option
    then Entry $ entry { expanded = not entry.expanded }
    else Entry $ entry
        { subEntries = toggleEntriesExpanded comparer option entry.subEntries }

handleAction
    :: forall option slots left
    .  (Action option)
    -> H.HalogenM (State option) (Action option) slots
        (Output option) (Async left) Unit
handleAction Initialize = do
    window <- H.liftEffect $ Window.toEventTarget <$> window
    let windowEventSource = ES.eventListenerEventSource
            (E.EventType "mousedown") window \_ -> Just TryClose
    windowSubscription <- H.subscribe $ ES.hoist affToAsync windowEventSource
    H.modify_ (_ { windowSubscription = Just windowSubscription })
handleAction Finalize = do
    { windowSubscription } <- H.get
    case windowSubscription of
        Just windowSubscription' -> H.unsubscribe windowSubscription'
        Nothing -> pure unit
handleAction Open =
    H.modify_ (_ { open = true, keepOpen = true })
handleAction Close =
    H.modify_ (_ { open = false })
handleAction KeepOpen =
    H.modify_ (_ { keepOpen = true })
handleAction TryClose =
    H.modify_ \state ->
        if state.keepOpen
        then state { keepOpen = false }
        else state { open = false }
handleAction (Filter text) =
    H.modify_ \state @ { entries, labeler, filter } ->
        state
        { entries = filterEntries text labeler entries
        , filter = filter { text = text }
        }
handleAction (SelectEntry option) = do
    state <- H.modify (_ { selected = option, open = false })
    H.raise $ SelectedChanged state.selected
handleAction (ToggleEntryExpanded option) =
    H.modify_ \state @ { entries, comparer } ->
        state { entries = toggleEntriesExpanded comparer option entries }

handleQuery
    :: forall option monad slots action send
    .  MonadEffect monad
    => Query option send
    -> H.HalogenM (State option) action slots (Output option) monad (Maybe send)
handleQuery (Clear send) = do
    { selected } <- H.modify (_ { selected = Nothing })
    H.raise $ SelectedChanged selected
    pure $ Just send

createEntry :: forall option. InputEntry option -> Entry option
createEntry (InputEntry { option, subEntries }) = Entry
    { option: option
    , shown: true
    , expanded: false
    , subEntries: subEntries <#> createEntry
    }

component :: forall option left.
    H.Component HH.HTML (Query option)
        (Input option) (Output option) (Async left)
component = H.mkComponent
    { initialState: \{ entries, selected, labeler, comparer, filter } ->
        { entries: entries <#> createEntry
        , selected
        , labeler
        , comparer
        , filter: { placeHolder: filter, text: "" }
        , open: false
        , keepOpen: false
        , windowSubscription: Nothing
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        , finalize = Just Finalize
        }
    }

singleTreeSelect
    :: forall children' slot children action left option
    .  Cons slot (Slot option) children' children
    => IsSymbol slot
    => SProxy slot
    -> Input option
    -> (Output option -> Maybe action)
    -> HH.ComponentHTML action children (Async left)
singleTreeSelect label input handleOutput =
    HH.slot label unit component input handleOutput
