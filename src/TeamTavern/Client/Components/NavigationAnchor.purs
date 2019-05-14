module TeamTavern.Client.Components.NavigationAnchor where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy)
import Effect.Class (class MonadEffect)
import Halogen (defaultEval, get, liftEffect, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML (a)
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (href)
import Prim.Row (class Cons)
import TeamTavern.Client.Script.Navigate (navigate_)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type State slots monad =
    { path :: String
    , content :: HH.ComponentHTML (Action slots monad) slots monad
    }

data Action slots monad = Navigate MouseEvent | Receive (State slots monad)

type Slot = H.Slot (Const Void) Void

render :: forall slots monad.
  State slots monad -> HH.ComponentHTML (Action slots monad) slots monad
render { path, content } =
    a [ href path, onClick $ Navigate >>> Just ] [ content ]

handleAction
    :: forall slots monad output
    .  MonadEffect monad
    => Action slots monad
    -> H.HalogenM (State slots monad) (Action slots monad)
        slots output monad Unit
handleAction (Navigate event) = do
    liftEffect $ preventDefault $ toEvent event
    { path } <- get
    liftEffect $ navigate_ path
    pure unit
handleAction (Receive state) = H.put state

component :: forall query slots monad output. MonadEffect monad =>
    H.Component HH.HTML query (State slots monad) output monad
component = mkComponent
    { initialState: identity
    , render
    , eval: mkEval $ defaultEval
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
    -> State children monad
    -> HH.ComponentHTML action children monad
navigationAnchor label state = HH.slot label unit component state absurd

navigationAnchorIndexed
    :: forall label children children' action monad index
    .  Cons label (Slot index) children' children
    => IsSymbol label
    => Ord index
    => MonadEffect monad
    => SProxy label
    -> index
    -> State children monad
    -> HH.ComponentHTML action children monad
navigationAnchorIndexed label index state =
    HH.slot label index component state absurd
