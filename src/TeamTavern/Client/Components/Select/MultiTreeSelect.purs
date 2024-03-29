module TeamTavern.Client.Components.Select.MultiTreeSelect
    (Labeler, Comparer, InputEntry(..), Input, Output, Slot, multiTreeSelect) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Foldable (all, any, intercalate)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), contains, toLower, trim)
import Data.String as String
import Data.String.Utils (repeat)
import Data.Symbol (class IsSymbol)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event as ES
import Halogen.Query.HalogenM (SubscriptionId)
import Prim.Row (class Cons)
import TeamTavern.Client.Components.Checkable (CheckboxState(..), checkbox')
import TeamTavern.Client.Components.Input (checkboxLabel)
import TeamTavern.Client.Shared.Slot (Slot_O_)
import Type.Proxy (Proxy)
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.Window as Window

newtype InputEntry option = InputEntry
    { option :: option
    , subEntries :: Array (InputEntry option)
    }

newtype Entry option = Entry
    { option :: option
    , state :: CheckboxState
    , shown :: Boolean
    , expanded :: Boolean
    , subEntries :: Array (Entry option)
    }

type Entries option = Array (Entry option)

type Labeler option = option -> String

type Comparer option = option -> option -> Boolean

type Input option =
    { entries :: Array (InputEntry option)
    , selected :: Array option
    , labeler :: Labeler option
    , comparer :: Comparer option
    , filter :: String
    }

type State option =
    { entries :: Entries option
    , labeler :: Labeler option
    , comparer :: Comparer option
    , filter :: { placeHolder :: String, text :: String }
    , open :: Boolean
    , keepOpen :: Boolean
    , windowSubscription :: Maybe SubscriptionId
    }

data Action option
    = Initialize
    | Receive (Input option)
    | Finalize
    | Open
    | Close
    | KeepOpen
    | TryClose
    | Filter String
    | ToggleEntryState option
    | ToggleEntryExpanded option

type Output option = Array option

type Slot option = Slot_O_ (Output option)

selectedEntriesText :: ∀ option. Labeler option -> Entries option -> String
selectedEntriesText labeler entries =
    entries
    <#> (\(Entry entry) ->
        case entry.state of
        Checked -> labeler entry.option
        Indeterminate -> selectedEntriesText labeler entry.subEntries
        Unchecked -> "")
    # Array.filter (_ /= "")
    # intercalate ", "

renderEntry
    :: ∀ option slots
    .  Int
    -> Labeler option
    -> Entry option
    -> Maybe (Array (HH.HTML slots (Action option)))
renderEntry _ _ (Entry { shown: false }) = Nothing
renderEntry level labeler (Entry { option, state, expanded, subEntries }) = let
    hasSubEntries = not $ Array.null subEntries
    optionClass =
            (if hasSubEntries then "collapsible-" else "")
            <> (maybe "" identity $ repeat level "sub-") <> "option"
    in
    Just $
    [ HH.div
        [ HP.class_ $ HH.ClassName optionClass
        , HE.onClick $ const $ ToggleEntryState option
        ]
        [ checkbox' state
        , checkboxLabel $ labeler option
        ]
    ]
    <>
    (if hasSubEntries
        then Array.singleton $
            HH.i
            [ HP.class_ $ HH.ClassName $ "fas "
                <> (if expanded then "fa-caret-up" else "fa-caret-down")
                <> " collapsible-option-caret"
            , HE.onMouseDown $ const $ ToggleEntryExpanded option
            ]
            []
        else [])
    <>
    if expanded
    then renderEntries (level + 1) labeler subEntries
    else []

renderEntries
    :: ∀ option slots
    .  Int
    -> Labeler option
    -> Entries option
    -> Array (HH.HTML slots (Action option))
renderEntries level labeler entries =
    entries <#> renderEntry level labeler # Array.catMaybes # join

render :: ∀ option slots. State option -> HH.HTML slots (Action option)
render { entries, labeler, filter, open } = let
    selectedClass = if open then "selected-open" else "selected-closed"
    openOrClose = if open then Close else Open
    in
    HH.div [ HP.class_ $ HH.ClassName "select" ] $
    [ HH.div
        [ HP.class_ $ HH.ClassName selectedClass
        , HE.onMouseDown $ const openOrClose
        ]
        [ HH.text $ selectedEntriesText labeler entries]
    ]
    <>
    if open
    then
        [ HH.div [ HP.class_ $ HH.ClassName "select-filter" ] $ pure $
            HH.input
            [ HP.class_ $ HH.ClassName "select-filter-input"
            , HP.placeholder filter.placeHolder
            , HP.value filter.text
            , HE.onMouseDown $ const KeepOpen
            , HE.onValueInput Filter
            ]
        , HH.div
            [ HP.class_ $ HH.ClassName "filterable-options"
            , HE.onMouseDown $ const KeepOpen
            ]
            (renderEntries 0 labeler entries)
        ]
    else []

filterEntries :: ∀ option.
    String -> Labeler option -> Entries option -> Entries option
filterEntries filter labeler entries | String.null $ trim filter =
    entries <#> \(Entry entry) -> Entry entry
        { shown = true
        , expanded = false
        , subEntries = filterEntries filter labeler entry.subEntries
        }
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

uncheckSubEntries :: ∀ option. Entries option -> Entries option
uncheckSubEntries subEntries =
    subEntries <#> \(Entry entry) -> Entry entry
        { state = Unchecked
        , subEntries = uncheckSubEntries entry.subEntries
        }

checkSubEntries :: ∀ option. Entries option -> Entries option
checkSubEntries subEntries =
    subEntries <#> \(Entry entry) -> Entry entry
        { state = Checked
        , subEntries = checkSubEntries entry.subEntries
        }

toggleEntriesState :: ∀ option.
    Comparer option -> option -> Entries option -> Entries option
toggleEntriesState comparer option entries = entries <#> \(Entry entry) ->
    if comparer option entry.option
    then
        case entry.state of
        Checked -> Entry entry
            { state = Unchecked
            , subEntries = uncheckSubEntries entry.subEntries
            }
        _ -> Entry entry
            { state = Checked
            , subEntries = checkSubEntries entry.subEntries
            }
    else if Array.null entry.subEntries
    then Entry entry
    else let
        subEntries = toggleEntriesState comparer option entry.subEntries
        allChecked = subEntries # all \(Entry subEntry) ->
            case subEntry.state of
            Checked -> true
            _ -> false
        someChecked = subEntries # any \(Entry subEntry) ->
            case subEntry.state of
            Unchecked -> false
            _ -> true
        in
        Entry entry
            { state =
                if allChecked
                then Checked
                else if someChecked
                then Indeterminate
                else Unchecked
            , subEntries = subEntries
            }

toggleEntriesExpanded :: ∀ option.
    Comparer option -> option -> Entries option -> Entries option
toggleEntriesExpanded comparer option entries = entries <#> \(Entry entry) ->
    if comparer option entry.option
    then Entry $ entry { expanded = not entry.expanded }
    else Entry $ entry
        { subEntries = toggleEntriesExpanded comparer option entry.subEntries }

getSelectedEntries :: ∀ option. Entries option -> Array option
getSelectedEntries entries =
    entries
    <#> (\(Entry entry) ->
        case entry.state of
        Checked -> Array.singleton entry.option
        Indeterminate -> getSelectedEntries entry.subEntries
        Unchecked -> [])
    # join

findMatchingEntry :: ∀ option. Comparer option -> Entry option -> Entries option -> Maybe (Entry option)
findMatchingEntry comparer (Entry newEntry) existingEntries =
    existingEntries # Array.findMap \(Entry existingEntry) ->
        if comparer newEntry.option existingEntry.option
        then Just $ Entry existingEntry
        else findMatchingEntry comparer (Entry newEntry) existingEntry.subEntries

setExistingExpanded :: ∀ option. Comparer option -> Entries option -> Entries option -> Entries option
setExistingExpanded comparer newEntries existingEntries = newEntries <#> \(Entry newEntry) ->
    let matchingEntry = findMatchingEntry comparer (Entry newEntry) existingEntries
    in
    case matchingEntry of
    Nothing -> Entry newEntry
    Just (Entry matchingEntry') ->
        Entry newEntry
            { expanded = if newEntry.shown then matchingEntry'.expanded else false
            , subEntries = setExistingExpanded comparer newEntry.subEntries matchingEntry'.subEntries
            }

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
        { entries =
            let
            checkedEntries =
                Array.foldr
                (toggleEntriesState input.comparer)
                (input.entries <#> createEntry)
                input.selected
            filteredEntries =
                filterEntries state.filter.text input.labeler checkedEntries
            in
            setExistingExpanded input.comparer filteredEntries state.entries
        , labeler = input.labeler
        , comparer = input.comparer
        , filter = { placeHolder: input.filter, text: state.filter.text }
        }
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
handleAction (ToggleEntryState option) = do
    state <- H.modify \state @ { entries, comparer } ->
        state { entries = toggleEntriesState comparer option entries }
    H.raise $ getSelectedEntries state.entries
handleAction (ToggleEntryExpanded option) =
    H.modify_ \state @ { entries, comparer } ->
        state { entries = toggleEntriesExpanded comparer option entries }

clearEntries :: ∀ option. Entries option -> Entries option
clearEntries entries = entries <#> \(Entry entry) ->
    Entry entry
        { state = Unchecked
        , subEntries = clearEntries entry.subEntries
        }

createEntry :: ∀ option. InputEntry option -> Entry option
createEntry (InputEntry { option, subEntries }) = Entry
    { option: option
    , state: Unchecked
    , shown: true
    , expanded: false
    , subEntries: subEntries <#> createEntry
    }

component :: ∀ query option left.
    H.Component query (Input option) (Output option) (Async left)
component = H.mkComponent
    { initialState: \{ entries, selected, labeler, comparer, filter } ->
        { entries:
            Array.foldr
            (toggleEntriesState comparer)
            (entries <#> createEntry)
            selected
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
        , initialize = Just Initialize
        , receive = Just <<< Receive
        , finalize = Just Finalize
        }
    }

multiTreeSelect
    :: ∀ children' slot children action left option
    .  Cons slot (Slot option) children' children
    => IsSymbol slot
    => Proxy slot
    -> Input option
    -> (Output option -> action)
    -> HH.ComponentHTML action children (Async left)
multiTreeSelect label input handleOutput =
    HH.slot label unit component input handleOutput
