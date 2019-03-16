module TeamTavern.Client.Components.Modal (Message, component) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..))
import Debug.Trace (traceM)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Unsafe.Reference (unsafeRefEq)
import Web.Event.Event (target)
import Web.HTML.HTMLElement (fromEventTarget)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

data Action output = Close MouseEvent | HandleInner output

data Message output = CloseMessage | Inner output

type ChildSlots output slots =
    (content :: H.Slot (Const Void) output Unit | slots)

renderModal
    :: forall slots output monad
    .  H.Component HH.HTML (Const Void) Unit output monad
    -> HH.ComponentHTML (Action output) (ChildSlots output slots) monad
renderModal content = HH.div
    [ HP.class_ $ H.ClassName "modal-background"
    , HP.ref $ H.RefLabel "modal-background"
    , HE.onClick $ Just <<< Close
    ]
    [ HH.slot (SProxy :: SProxy "content") unit
        content unit (Just <<< HandleInner) ]

handleAction :: forall state action slots output monad.
    Action output -> H.HalogenM state action slots (Message output) monad Unit
handleAction (Close event) = do
    traceM "wtf"
    background <- H.getHTMLElementRef (H.RefLabel "modal-background")
    eventTarget <- event # toEvent # target >>= fromEventTarget # pure
    case background, eventTarget of
        Just background', Just eventTarget'
            | unsafeRefEq background' eventTarget' -> H.raise CloseMessage
        _, _ -> pure unit
handleAction (HandleInner message) = H.raise $ Inner message

component
    :: forall query input output monad
    .  H.Component HH.HTML (Const Void) Unit output monad
    -> H.Component HH.HTML query input (Message output) monad
component content = H.mkComponent
    { initialState: identity
    , render: const $ renderModal content
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction }
    }
