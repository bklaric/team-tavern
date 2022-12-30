module TeamTavern.Client.Components.Modal (Output(..), component) where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import TeamTavern.Client.Script.Unscrollable (makeWindowScrollable, makeWindowUnscrollable)
import TeamTavern.Client.Snippets.Class as HS
import Type.Proxy (Proxy(..))
import Unsafe.Reference (unsafeRefEq)
import Web.Event.Event as Event
import Web.HTML.HTMLElement as HtmlElement
import Web.UIEvent.MouseEvent as MouseEvent

data Output output = CloseClicked | OutputRaised output

type ChildSlots query output = (content :: H.Slot query output Unit)

type Slot output = H.Slot (Const Void) output Unit

component
    :: ∀ query' monad output query input
    .  MonadEffect monad
    => String
    -> H.Component query input output monad
    -> H.Component query' input (Output output) monad
component title content = Hooks.component \{ outputToken } input -> Hooks.do
    Hooks.useLifecycleEffect do
        makeWindowUnscrollable
        pure $ Just makeWindowScrollable
    let onBackgroundClick event = do
            background <- Hooks.getHTMLElementRef (H.RefLabel "modal-background")
            let eventTarget = event # MouseEvent.toEvent # Event.target >>= HtmlElement.fromEventTarget
            case background, eventTarget of
                Just background', Just eventTarget'
                    | unsafeRefEq background' eventTarget' -> Hooks.raise outputToken CloseClicked
                _, _ -> pure unit
    let onCloseClick = Hooks.raise outputToken CloseClicked
    let onRaiseOutput message = Hooks.raise outputToken $ OutputRaised message
    Hooks.pure $ HH.div
        [ HP.class_ $ H.ClassName "modal-background"
        , HP.ref $ H.RefLabel "modal-background"
        , HE.onClick onBackgroundClick
        ]
        [ HH.div [ HS.class_ "modal-content" ]
            [ HH.h1 [ HS.class_ "modal-title" ]
                [ HH.text title
                , HH.button
                    [ HS.class_ "modal-close-button"
                    , HE.onClick $ const onCloseClick
                    ]
                    [ HH.i [ HS.class_ "fas fa-times modal-close-button-icon" ] [] ]
                ]
            , HH.slot (Proxy :: _ "content") unit content input onRaiseOutput
            ]
        ]
