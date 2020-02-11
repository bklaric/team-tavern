module TeamTavern.Client.Components.SingleSelect where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Array as Array
import Data.Foldable (find)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), contains, toLower, trim)
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)

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
    , keepOpenInput :: Boolean
    , keepOpenOptions :: Boolean
    }

data Action option
    = Select (Maybe option)
    | Open
    | Close
    | TryClose
    | KeepOpenInput Boolean
    | KeepOpenOptions Boolean
    | FilterInput String

data Query option send = Selected (Maybe option -> send)

data Message option = SelectedChanged (Maybe option)

type Slot option = H.Slot (Query option) (Message option)

render :: forall slots option. State option -> HH.HTML slots (Action option)
render { options, selected, labeler, showFilter, open } =
    HH.div
    [ HP.class_ $ ClassName "select"
    , HP.tabIndex 0
    , HE.onFocus $ const $ Just Open
    , HE.onFocusOut $ const $ Just TryClose
    ]
    $ [ HH.div
        [ HP.class_ $ ClassName
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
                    , HE.onMouseDown $ const $ Just $ KeepOpenInput true
                    , HE.onBlur $ const $ Just $ KeepOpenInput false
                    , HE.onValueInput $ Just <<< FilterInput
                    ]
                ])
            <>
            [ HH.div
                [ HP.class_ $ HH.ClassName
                    if isJust showFilter
                    then "filterable-options"
                    else "options"
                , HP.tabIndex $ -1
                , HE.onMouseDown $ const $ Just $ KeepOpenOptions true
                , HE.onBlur $ const $ Just $ KeepOpenOptions false
                ] $
                [ HH.div
                    [ HP.class_ $ ClassName "option"
                    , HE.onClick $ const $ Just $ Select Nothing
                    ]
                    [ HH.text "" ]
                ]
                <>
                (options # Array.filter _.shown <#> \option ->
                    HH.div
                    [ HP.class_ $ ClassName "option"
                    , HE.onClick $ const $ Just $ Select $ Just $ option.option
                    ]
                    [ HH.text $ labeler option.option ])
            ]
        else
            []

handleAction :: forall option slots monad.
    (Action option) -> H.HalogenM (State option) (Action option) slots (Message option) monad Unit
handleAction (Select selected) = do
    H.modify_ (_ { selected = selected, open = false })
    H.raise $ SelectedChanged selected
handleAction Open =
    H.modify_ \state -> state
        { options = state.options <#> (_ { shown = true })
        , open = true
        }
handleAction Close =
    H.modify_ \state -> state { open = false }
handleAction TryClose = do
    H.modify_ \state ->
        if state.keepOpenInput || state.keepOpenOptions
        then state
        else state { open = false }
handleAction (KeepOpenInput keepOpen) =
    H.modify_ (_ { keepOpenInput = keepOpen })
handleAction (KeepOpenOptions keepOpen) = do
    H.modify_ (_ { keepOpenOptions = keepOpen })
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

component :: forall option monad.
    H.Component HH.HTML (Query option) (Input option) (Message option) monad
component = H.mkComponent
    { initialState: \{ options, selected, labeler, comparer, showFilter } ->
        { options: options <#> { option: _, shown: true }
        , selected: selected >>= \selected' ->
            options # find (\option -> comparer option selected')
        , labeler
        , comparer
        , showFilter
        , open: false
        , keepOpenInput: false
        , keepOpenOptions: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

singleSelect
    :: forall children' slot children action monad option
    .  Cons slot (Slot option Unit) children' children
    => IsSymbol slot
    => SProxy slot
    -> Input option
    -> HH.HTML (H.ComponentSlot HH.HTML children monad action) action
singleSelect label input = HH.slot label unit component input (const Nothing)

singleSelect'
    :: forall children' slot children action monad option
    .  Cons slot (Slot option Unit) children' children
    => IsSymbol slot
    => SProxy slot
    -> Input option
    -> (Message option -> Maybe action)
    -> HH.HTML (H.ComponentSlot HH.HTML children monad action) action
singleSelect' label input handleMessage =
    HH.slot label unit component input handleMessage

singleSelectIndexed
    :: forall children' slot children action monad option index
    .  Cons slot (Slot option index) children' children
    => IsSymbol slot
    => Ord index
    => SProxy slot
    -> index
    -> Input option
    -> HH.HTML (H.ComponentSlot HH.HTML children monad action) action
singleSelectIndexed label index input =
    HH.slot label index component input (const Nothing)
