module TeamTavern.Client.Components.ModalDeclarative
    (Output(..), component) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Script.Unscrollable (makeWindowScrollable, makeWindowUnscrollable)
import Unsafe.Reference (unsafeRefEq)
import Web.Event.Event (target)
import Web.HTML.HTMLElement (fromEventTarget)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Input input = input

type State input = Input input

data Action output
    = Init
    | Finalize
    | BackgroundClick MouseEvent
    | OutputRaise output

data Output output = BackgroundClicked | OutputRaised output

type ChildSlots query output = (content :: H.Slot query output Unit)

render
    :: forall query input output monad
    .  H.Component HH.HTML query input output monad
    -> State input
    -> HH.ComponentHTML (Action output) (ChildSlots query output) monad
render content input =
    HH.div
    [ HP.class_ $ H.ClassName "modal-background"
    , HP.ref $ H.RefLabel "modal-background"
    , HE.onMouseDown $ Just <<< BackgroundClick
    ]
    [ HH.slot (SProxy :: SProxy "content") unit
        content input (Just <<< OutputRaise)
    ]

handleAction :: forall state action slots output monad. MonadEffect monad =>
    Action output -> H.HalogenM state action slots (Output output) monad Unit
handleAction Init =
    makeWindowUnscrollable
handleAction Finalize =
    makeWindowScrollable
handleAction (BackgroundClick event) = do
    background <- H.getHTMLElementRef (H.RefLabel "modal-background")
    eventTarget <- event # toEvent # target >>= fromEventTarget # pure
    case background, eventTarget of
        Just background', Just eventTarget'
            | unsafeRefEq background' eventTarget' -> H.raise BackgroundClicked
        _, _ -> pure unit
handleAction (OutputRaise message) = H.raise $ OutputRaised message

component
    :: forall query input output monad
    .  MonadEffect monad
    => H.Component HH.HTML query input output monad
    -> H.Component HH.HTML query input (Output output) monad
component content = H.mkComponent
    { initialState: identity
    , render: render content
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , finalize = Just Finalize
        }
    }
