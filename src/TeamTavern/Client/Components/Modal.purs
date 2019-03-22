module TeamTavern.Client.Components.Modal
    (Query(..), Message(..), component, hide, show, showWith) where

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

data State input = Shown input | Hidden

data Action output
    = BackgroundClick MouseEvent
    | InnerMessage output

data Query input query send
    = Show input send
    | Hide send
    | InnerQuery (query send)

data Message output = BackgroundClicked | Inner output

type ChildSlots query output = (content :: H.Slot query output Unit)

renderModal
    :: forall query input output monad
    .  H.Component HH.HTML query input output monad
    -> State input
    -> HH.ComponentHTML (Action output) (ChildSlots query output) monad
renderModal _ Hidden = HH.div_ []
renderModal content (Shown input) = HH.div
    [ HP.class_ $ H.ClassName "modal-background"
    , HP.ref $ H.RefLabel "modal-background"
    , HE.onClick $ Just <<< BackgroundClick
    ]
    [ HH.slot (SProxy :: SProxy "content") unit
        content input (Just <<< InnerMessage) ]

handleAction :: forall state action slots output monad. MonadEffect monad =>
    Action output -> H.HalogenM state action slots (Message output) monad Unit
handleAction (BackgroundClick event) = do
    background <- H.getHTMLElementRef (H.RefLabel "modal-background")
    eventTarget <- event # toEvent # target >>= fromEventTarget # pure
    case background, eventTarget of
        Just background', Just eventTarget'
            | unsafeRefEq background' eventTarget' -> H.raise BackgroundClicked
        _, _ -> pure unit
handleAction (InnerMessage message) = H.raise $ Inner message

handleQuery
    :: forall query input output monad send
    .  MonadEffect monad
    => Query input query send
    -> H.HalogenM (State input) (Action output) (ChildSlots query output)
        (Message output) monad (Maybe send)
handleQuery (Show input send) = do
    window >>= document >>= body
        >>= (maybe (pure unit) (setClassName "unscrollable")) # H.liftEffect
    H.put (Shown input)
    pure $ Just send
handleQuery (Hide send) = do
    window >>= document >>= body
        >>= (maybe (pure unit) (setClassName "")) # H.liftEffect
    H.put Hidden $> Just send
handleQuery (InnerQuery send) = H.query (SProxy :: SProxy "content") unit send

component
    :: forall query input modalInput output monad
    .  MonadEffect monad
    => H.Component HH.HTML query input output monad
    -> H.Component HH.HTML (Query input query) modalInput (Message output) monad
component content = H.mkComponent
    { initialState: const Hidden
    , render: renderModal content
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

show
    :: forall output action state query children' index children slot monad
    .  Cons slot (H.Slot (Query Unit query) index Unit) children' children
    => IsSymbol slot
    => SProxy slot
    -> H.HalogenM state action children output monad Unit
show slot = void $ H.query slot unit (Show unit unit)

showWith
    :: forall input output action state query children' index children slot monad
    .  Cons slot (H.Slot (Query input query) index Unit) children' children
    => IsSymbol slot
    => input
    -> SProxy slot
    -> H.HalogenM state action children output monad Unit
showWith input slot = void $ H.query slot unit (Show input unit)

hide
    :: forall input children' index children slot monad output action state query
    .  Cons slot (H.Slot (Query input query) index Unit) children' children
    => IsSymbol slot
    => SProxy slot
    -> H.HalogenM state action children output monad Unit
hide slot = void $ H.query slot unit (Hide unit)
