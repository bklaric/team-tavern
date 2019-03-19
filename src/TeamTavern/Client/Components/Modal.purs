module TeamTavern.Client.Components.Modal
    (Query(..), Message(..), component, hide, show) where

import Prelude
import Prim.Row (class Cons)

import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Unsafe.Reference (unsafeRefEq)
import Web.Event.Event (target)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (fromEventTarget, setClassName)
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data State = Shown | Hidden

data Action output
    = Init
    | Final
    | BackgroundClick MouseEvent
    | InnerMessage output

data Query inner send = Show send | Hide send | InnerQuery (inner send)

data Message output = BackgroundClicked | Inner output

type ChildSlots query output = (content :: H.Slot query output Unit)

renderModal
    :: forall query output monad
    .  H.Component HH.HTML query Unit output monad
    -> State
    -> HH.ComponentHTML (Action output) (ChildSlots query output) monad
renderModal _ Hidden = HH.div_ []
renderModal content Shown = HH.div
    [ HP.class_ $ H.ClassName "modal-background"
    , HP.ref $ H.RefLabel "modal-background"
    , HE.onClick $ Just <<< BackgroundClick
    ]
    [ HH.slot (SProxy :: SProxy "content") unit
        content unit (Just <<< InnerMessage) ]

handleAction :: forall state action slots output monad. MonadEffect monad =>
    Action output -> H.HalogenM state action slots (Message output) monad Unit
handleAction Init =
    window >>= document >>= body
        >>= (maybe (pure unit) (setClassName "unscrollable")) # H.liftEffect
handleAction Final =
    window >>= document >>= body
        >>= (maybe (pure unit) (setClassName "")) # H.liftEffect
handleAction (BackgroundClick event) = do
    background <- H.getHTMLElementRef (H.RefLabel "modal-background")
    eventTarget <- event # toEvent # target >>= fromEventTarget # pure
    case background, eventTarget of
        Just background', Just eventTarget'
            | unsafeRefEq background' eventTarget' -> H.raise BackgroundClicked
        _, _ -> pure unit
handleAction (InnerMessage message) = H.raise $ Inner message

handleQuery
  :: forall query output monad send
   . Query query send
  -> H.HalogenM State (Action output) (ChildSlots query output)
    (Message output) monad (Maybe send)
handleQuery (Show send) = H.put Shown $> Just send
handleQuery (Hide send) = H.put Hidden $> Just send
handleQuery (InnerQuery send) = H.query (SProxy :: SProxy "content") unit send

component
    :: forall query input output monad
    .  MonadEffect monad
    => H.Component HH.HTML query Unit output monad
    -> H.Component HH.HTML (Query query) input (Message output) monad
component content = H.mkComponent
    { initialState: const Hidden
    , render: renderModal content
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Init
        , finalize = Just Final
        }
    }

show
    :: forall output action state inner children' index children slot monad
    .  Cons slot (H.Slot (Query inner) index Unit) children' children
    => IsSymbol slot
    => SProxy slot
    -> H.HalogenM state action children output monad Unit
show slot = void $ H.query slot unit (Show unit)

hide
    :: forall children' index children slot monad output action state inner
    .  Cons slot (H.Slot (Query inner) index Unit) children' children
    => IsSymbol slot
    => SProxy slot
    -> H.HalogenM state action children output monad Unit
hide slot = void $ H.query slot unit (Hide unit)
