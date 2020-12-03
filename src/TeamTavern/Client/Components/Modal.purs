module TeamTavern.Client.Components.Modal (Output(..), component) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Script.Unscrollable (makeWindowScrollable, makeWindowUnscrollable)
import TeamTavern.Client.Snippets.Class as HS
import Unsafe.Reference (unsafeRefEq)
import Web.Event.Event (target)
import Web.HTML.HTMLElement (fromEventTarget)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Input input = input

type State input = Input input

data Action output
    = Init
    | Finalize
    | BackgroundClose MouseEvent
    | ButtonClose
    | OutputRaise output

data Output output = CloseClicked | OutputRaised output

type ChildSlots query output = (content :: H.Slot query output Unit)

type Slot output = H.Slot (Const Void) output Unit

render
    :: forall query input output monad
    .  String
    -> H.Component HH.HTML query input output monad
    -> State input
    -> HH.ComponentHTML (Action output) (ChildSlots query output) monad
render title content input =
    HH.div
    [ HP.class_ $ H.ClassName "modal-background"
    , HP.ref $ H.RefLabel "modal-background"
    , HE.onClick $ Just <<< BackgroundClose
    ]
    [ HH.div [ HS.class_ "modal-content" ]
        [ HH.h1 [ HS.class_ "modal-title" ]
            [ HH.text title
            , HH.button
                [ HS.class_ "modal-close-button"
                , HE.onClick $ const $ Just ButtonClose
                ]
                [ HH.i [ HS.class_ "fas fa-times modal-close-button-icon" ] [] ]
            ]
        , HH.slot (SProxy :: SProxy "content") unit content input (Just <<< OutputRaise)
        ]
    ]

handleAction :: forall state action slots output monad. MonadEffect monad =>
    Action output -> H.HalogenM state action slots (Output output) monad Unit
handleAction Init =
    makeWindowUnscrollable
handleAction Finalize =
    makeWindowScrollable
handleAction (BackgroundClose event) = do
    background <- H.getHTMLElementRef (H.RefLabel "modal-background")
    eventTarget <- event # toEvent # target >>= fromEventTarget # pure
    case background, eventTarget of
        Just background', Just eventTarget'
            | unsafeRefEq background' eventTarget' -> H.raise CloseClicked
        _, _ -> pure unit
handleAction ButtonClose = H.raise CloseClicked
handleAction (OutputRaise message) = H.raise $ OutputRaised message

component
    :: forall query input output monad
    .  MonadEffect monad
    => String
    -> H.Component HH.HTML query input output monad
    -> H.Component HH.HTML query input (Output output) monad
component title content = H.mkComponent
    { initialState: identity
    , render: render title content
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , finalize = Just Finalize
        }
    }
