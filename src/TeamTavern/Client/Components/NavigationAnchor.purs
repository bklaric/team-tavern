module TeamTavern.Client.Components.NavigationAnchor where

import Prelude
import Prim.Row (class Cons)

import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy)
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, get, liftEffect)
import Halogen as H
import Halogen.HTML (HTML, a)
import Halogen.HTML as HH
import Halogen.HTML as Html
import Halogen.HTML.Events (input, onClick)
import Halogen.HTML.Properties (href)
import TeamTavern.Client.Script.Navigate (navigate_)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Query send = Navigate MouseEvent send

type State = { path :: String, text :: String }

type Message = Void

type Slot = H.Slot Query Message

render :: forall m. State -> ComponentHTML Query () m
render { path, text } = a
    [ href path, onClick $ input Navigate ]
    [ Html.text text ]

eval :: forall m. MonadEffect m => Query ~> H.HalogenM State Query () Message m
eval (Navigate event send) = do
    liftEffect $ preventDefault $ toEvent event
    { path } <- get
    liftEffect $ navigate_ path
    pure send

component :: forall m. MonadEffect m => Component HTML Query State Void m
component = H.component
    { initialState: identity
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }

navigationAnchor
    :: forall query monad children children' label
    .  Cons label (Slot Unit) children' children
    => IsSymbol label
    => MonadEffect monad
    => SProxy label
    -> State
    -> HH.ComponentHTML query children monad
navigationAnchor label state = HH.slot label unit component state absurd

navigationAnchorIndexed
    :: forall query monad index children children' label
    .  Cons label (Slot index) children' children
    => IsSymbol label
    => Ord index
    => MonadEffect monad
    => SProxy label
    -> index
    -> State
    -> HH.ComponentHTML query children monad
navigationAnchorIndexed label index state =
    HH.slot label index component state absurd
