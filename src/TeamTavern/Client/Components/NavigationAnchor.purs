module TeamTavern.Client.Components.NavigationAnchor
    ( Action, SimpleInput, ClassInput, Slot
    , navigationAnchor, navigationAnchorClassed, navigationAnchorIndexed) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Snippets.PreventMouseDefault (preventMouseDefault)
import Web.UIEvent.MouseEvent (MouseEvent)

type SimpleInput slots monad =
    { path :: String
    , content :: HH.ComponentHTML (Action slots monad) slots monad
    }

type ClassInput slots monad =
    { class_ :: String
    , path :: String
    , content :: HH.ComponentHTML (Action slots monad) slots monad
    }

type State slots monad = ClassInput slots monad

data Action slots monad = Navigate MouseEvent | Receive (State slots monad)

type Slot = H.Slot (Const Void) Void

render :: forall slots monad.
  State slots monad -> HH.ComponentHTML (Action slots monad) slots monad
render { class_, path, content } =
    HH.a
    [ HP.class_ $ HH.ClassName class_
    , HP.href path
    , HE.onClick $ Navigate >>> Just
    ]
    [ content ]

handleAction
    :: forall slots monad output
    .  MonadEffect monad
    => Action slots monad
    -> H.HalogenM (State slots monad) (Action slots monad)
        slots output monad Unit
handleAction (Navigate event) = do
    preventMouseDefault event
    { path } <- H.get
    navigate_ path
handleAction (Receive state) = H.put state

component :: forall query slots monad output. MonadEffect monad =>
    H.Component HH.HTML query (State slots monad) output monad
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

navigationAnchor
    :: forall label children children' action monad
    .  Cons label (Slot Unit) children' children
    => IsSymbol label
    => MonadEffect monad
    => SProxy label
    -> SimpleInput children monad
    -> HH.ComponentHTML action children monad
navigationAnchor label { path, content } =
    HH.slot label unit component { class_: "", path, content } absurd

navigationAnchorClassed
    :: forall label children children' action monad
    .  Cons label (Slot Unit) children' children
    => IsSymbol label
    => MonadEffect monad
    => SProxy label
    -> ClassInput children monad
    -> HH.ComponentHTML action children monad
navigationAnchorClassed label state =
    HH.slot label unit component state absurd

navigationAnchorIndexed
    :: forall label children children' action monad index
    .  Cons label (Slot index) children' children
    => IsSymbol label
    => Ord index
    => MonadEffect monad
    => SProxy label
    -> index
    -> SimpleInput children monad
    -> HH.ComponentHTML action children monad
navigationAnchorIndexed label index { path, content } =
    HH.slot label index component { class_: "", path, content } absurd
