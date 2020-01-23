module TeamTavern.Client.Components.MultiSelect where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), contains, toLower, trim)
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import Web.Event.Event (preventDefault)
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
    , keepOpenInput :: Boolean
    , keepOpenOptions :: Boolean
    }

data Action option
    = ToggleOption option
    | Open
    | Close
    | TryClose
    | PreventDefault MouseEvent
    | KeepOpenInput Boolean
    | KeepOpenOptions Boolean
    | FilterInput String

data Query option send
    = Selected (Array option -> send)
    | Clear send

type Slot option = H.Slot (Query option) Void

render :: forall slots option. State option -> HH.HTML slots (Action option)
render { options, labeler, comparer, showFilter, open } =
    HH.div
    [ HP.class_ $ HH.ClassName "select"
    , HP.tabIndex 0
    , HE.onFocusOut $ const $ Just TryClose
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

handleAction :: forall option slots message monad. MonadEffect monad =>
    (Action option) -> H.HalogenM (State option) (Action option) slots message monad Unit
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
        }
handleAction Close =
    H.modify_ \state -> state { open = false }
handleAction TryClose = do
    H.modify_ \state ->
        if state.keepOpenInput || state.keepOpenOptions
        then state
        else state { open = false }
handleAction (PreventDefault mouseEvent) =
    H.liftEffect $ preventDefault (MouseEvent.toEvent mouseEvent)
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
    { options } <- H.get
    pure $ Just $ send (options # Array.filter (_.selected) <#> _.option)
handleQuery (Clear send) = do
    H.modify_ \state @ { options } ->
        state { options = options <#> \option ->
            option { selected = false }
        }
    pure $ Just send

component :: forall option message monad. MonadEffect monad =>
    H.Component HH.HTML (Query option) (Input option) message monad
component = H.mkComponent
    { initialState: \{ options, labeler, comparer, showFilter } ->
        { options: options <#> \{ option, selected } ->
            { option, selected, shown: true }
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

multiSelect
    :: forall children' slot children output monad option
    .  Cons slot (Slot option Unit) children' children
    => IsSymbol slot
    => MonadEffect monad
    => SProxy slot
    -> Input option
    -> HH.HTML (H.ComponentSlot HH.HTML children monad output) output
multiSelect label input = HH.slot label unit component input absurd

multiSelectIndexed
    :: forall children' slot children output monad option index
    .  Cons slot (Slot option index) children' children
    => IsSymbol slot
    => Ord index
    => MonadEffect monad
    => SProxy slot
    -> index
    -> Input option
    -> HH.HTML (H.ComponentSlot HH.HTML children monad output) output
multiSelectIndexed label index input =
    HH.slot label index component input absurd
