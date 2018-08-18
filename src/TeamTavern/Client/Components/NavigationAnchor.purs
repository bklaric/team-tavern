module TeamTavern.Client.Components.NavigationAnchor where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen (Component, ComponentHTML, HalogenM, component, get, liftEffect)
import Halogen as Halogen
import Halogen.HTML (HTML, a)
import Halogen.HTML as Html
import Halogen.HTML.Events (input, onClick)
import Halogen.HTML.Properties (href)
import TeamTavern.Client.Script.Navigate (navigate)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Query send = Navigate MouseEvent send

type State = { path :: String, text :: String }

type Message = Void

type Slot = Halogen.Slot Query Message

render :: forall m. State -> ComponentHTML Query () m
render { path, text } = a
    [ href path, onClick $ input Navigate ]
    [ Html.text text ]

eval :: forall m. MonadEffect m => Query ~> HalogenM State Query () Message m
eval (Navigate event send) = do
    liftEffect $ preventDefault $ toEvent event
    { path } <- get
    liftEffect $ navigate path
    pure send

navigationAnchor :: forall m. MonadEffect m => Component HTML Query State Void m
navigationAnchor = component
    { initialState: identity
    , render
    , eval
    , receiver: const Nothing
    , initializer: Nothing
    , finalizer: Nothing
    }
