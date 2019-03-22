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
import Halogen.HTML as Html
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (href)
import Prim.Row (class Cons)
import TeamTavern.Client.Script.Navigate (navigate_)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Action = Navigate MouseEvent

type State = { path :: String, text :: String }

type Slot = H.Slot (Const Void) Void

render :: forall slots. State -> HH.HTML slots Action
render { path, text } = a
    [ href path, onClick $ Navigate >>> Just ]
    [ Html.text text ]

handleAction :: forall output monad. MonadEffect monad =>
    Action -> H.HalogenM State Action () output monad Unit
handleAction (Navigate event) = do
    liftEffect $ preventDefault $ toEvent event
    { path } <- get
    liftEffect $ navigate_ path
    pure unit

component :: forall query output monad. MonadEffect monad =>
    H.Component HH.HTML query State output monad
component = mkComponent
    { initialState: identity
    , render
    , eval: mkEval $ defaultEval { handleAction = handleAction }
    }

navigationAnchor
    :: forall label children children' action monad
    .  Cons label (Slot Unit) children' children
    => IsSymbol label
    => MonadEffect monad
    => SProxy label
    -> State
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
    -> State
    -> HH.ComponentHTML action children monad
navigationAnchorIndexed label index state =
    HH.slot label index component state absurd
