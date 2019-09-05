module TeamTavern.Client.Components.MultiSelect where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)

type Option option =
    { option :: option
    , selected :: Boolean
    }

type Input option =
    { options :: Array (Option option)
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    }

type State option =
    { options :: Array (Option option)
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    , open :: Boolean
    }

data Action option
    = ToggleOption option
    | Open
    | Close
    | Toggle

data Query option send
    = Selected (Array option -> send)
    | Clear send

type Slot option = H.Slot (Query option) Void

render :: forall slots option. State option -> HH.HTML slots (Action option)
render { options, labeler, comparer, open } =
    HH.div
    [ HP.class_ $ ClassName "select"
    , HP.tabIndex 0
    , HE.onBlur $ const $ Just Close
    ]
    $ [ HH.div
        [ HP.class_ $ ClassName
            if open then "selected-open" else "selected-closed"
        , HE.onMouseDown $ const $ Just Toggle
        ]
        [ HH.text $ intercalate ", "
            (options # Array.filter (_.selected) <#> (_.option >>> labeler))
        ]
    ]
    <> if open
        then
            [ HH.div
                [ HP.class_ $ ClassName "options" ]
                (options <#> \{ option, selected } ->
                    HH.div
                    [ HP.class_ $ ClassName "option"
                    , HE.onClick $ const $ Just $ ToggleOption option
                    ]
                    [ HH.input
                        [ HP.type_ InputCheckbox
                        , HP.checked selected
                        , HP.tabIndex $ -1
                        ]
                    , HH.text $ labeler option
                    ])
            ]
        else
            []

handleAction :: forall option slots message monad.
    (Action option) -> H.HalogenM (State option) (Action option) slots message monad Unit
handleAction (ToggleOption toggledOption) =
    H.modify_ (\state -> state
        { options = state.options <#> \{ option, selected } ->
            if state.comparer option toggledOption
            then { option, selected: not selected }
            else { option, selected }
        })
handleAction Open =
    H.modify_ \state -> state { open = true }
handleAction Close =
    H.modify_ \state -> state { open = false }
handleAction Toggle =
    H.modify_ \state -> state { open = not state.open }

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

component :: forall option message monad.
    H.Component HH.HTML (Query option) (Input option) message monad
component = H.mkComponent
    { initialState: \{ options, labeler, comparer } ->
        { options, labeler, comparer, open: false }
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
    => SProxy slot
    -> Input option
    -> HH.HTML (H.ComponentSlot HH.HTML children monad output) output
multiSelect label input = HH.slot label unit component input absurd

multiSelectIndexed
    :: forall children' slot children output monad option index
    .  Cons slot (Slot option index) children' children
    => IsSymbol slot
    => Ord index
    => SProxy slot
    -> index
    -> Input option
    -> HH.HTML (H.ComponentSlot HH.HTML children monad output) output
multiSelectIndexed label index input =
    HH.slot label index component input absurd
