module TeamTavern.Client.Components.SingleSelect where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)

type Input option =
    { options :: Array option
    , selected :: Maybe option
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    }

type State option =
    { options :: Array option
    , selected :: Maybe option
    , labeler :: option -> String
    , comparer :: option -> option -> Boolean
    , open :: Boolean
    }

data Action option = Select (Maybe option) | Open | Close | Toggle

data Query option send = Selected (Maybe option -> send)

type Slot option = H.Slot (Query option) Void

render :: forall slots option. State option -> HH.HTML slots (Action option)
render { options, selected, labeler, open } =
    HH.div
    [ HP.class_ $ ClassName "select"
    , HP.tabIndex 0
    , HE.onFocus $ const $ Just Open
    , HE.onBlur $ const $ Just Close
    ]
    $ [ HH.div
        [ HP.class_ $ ClassName
            if open then "selected-open" else "selected-closed"
        , HE.onMouseDown $ const $ Just Toggle
        ]
        [ HH.text
            case selected of
            Nothing -> ""
            Just option -> labeler option
        ]
    ]
    <> if open
        then
            [ HH.div
            [ HP.class_ $ ClassName "options" ]
            $ [ HH.div
                [ HP.class_ $ ClassName "option"
                , HE.onClick $ const $ Just $ Select Nothing
                ]
                [ HH.text "" ]
            ]
            <>
            (options <#> \option ->
                HH.div
                [ HP.class_ $ ClassName "option"
                , HE.onClick $ const $ Just $ Select $ Just $ option
                ]
                [ HH.text $ labeler option ])
            ]
        else
            []

handleAction :: forall option slots message monad.
    (Action option) -> H.HalogenM (State option) (Action option) slots message monad Unit
handleAction (Select selected) =
    H.modify_ (_ { selected = selected, open = false })
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
    { selected } <- H.get
    pure $ Just $ send selected

component :: forall option message monad.
    H.Component HH.HTML (Query option) (Input option) message monad
component = H.mkComponent
    { initialState: \{ options, selected, labeler, comparer } ->
        { options
        , selected: selected >>= \selected' ->
            options # find (\option -> comparer option selected')
        , labeler
        , comparer
        , open: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

singleSelect
    :: forall children' slot children output monad option
    .  Cons slot (Slot option Unit) children' children
    => IsSymbol slot
    => SProxy slot
    -> Input option
    -> HH.HTML (H.ComponentSlot HH.HTML children monad output) output
singleSelect label input = HH.slot label unit component input absurd

singleSelectIndexed
    :: forall children' slot children output monad option index
    .  Cons slot (Slot option index) children' children
    => IsSymbol slot
    => Ord index
    => SProxy slot
    -> index
    -> Input option
    -> HH.HTML (H.ComponentSlot HH.HTML children monad output) output
singleSelectIndexed label index input =
    HH.slot label index component input absurd
