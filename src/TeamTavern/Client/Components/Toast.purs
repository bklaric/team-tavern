module TeamTavern.Client.Components.Toast
    ( Toast
    , UseToast
    , toasts
    , useToast
    ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Effect.Timer (clearTimeout, setTimeout)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks (type (<>), Hook, HookM, HookType, Pure, UseRef, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Hook (class HookNewtype)
import Halogen.Subscription as Subscription
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button)
import TeamTavern.Client.Snippets.Class as HS

-- A line at the bottom of the screen after an action, with an action such as
-- Undo where the action can be undone.
type Toast m = { text :: String, action :: Maybe { label :: String, onAction :: HookM m Unit } }

foreign import data UseToast :: (Type -> Type) -> HookType

instance HookNewtype (UseToast m) (UseState (Maybe (Toast m)) <> UseRef (Maybe H.SubscriptionId) <> Pure)

-- One toast at a time: showing another replaces it. It goes after 8 seconds
-- when it has an action to take and after 5 when it only says something.
useToast :: ∀ m. MonadEffect m =>
    Hook m (UseToast m)
    { toast :: Maybe (Toast m), showToast :: Toast m -> HookM m Unit, dismissToast :: HookM m Unit }
useToast = Hooks.wrap Hooks.do
    toast /\ toastId <- Hooks.useState Nothing
    _ /\ timerRef <- Hooks.useRef Nothing

    let stopTimer = liftEffect (Ref.read timerRef) >>= traverse_ Hooks.unsubscribe
        dismissToast = do
            stopTimer
            Hooks.put toastId Nothing
        showToast toast' = do
            stopTimer
            Hooks.put toastId $ Just toast'
            let milliseconds = maybe 5000 (const 8000) toast'.action
            timer <- Hooks.subscribe $ Subscription.makeEmitter \push -> do
                timeout <- setTimeout milliseconds $ push dismissToast
                pure $ clearTimeout timeout
            liftEffect $ Ref.write (Just timer) timerRef

    Hooks.pure { toast, showToast, dismissToast }

-- Where the toast shows, announced as it appears. Its action dismisses it
-- first, so the action can show a toast of its own.
toasts :: ∀ w m. Maybe (Toast m) -> HookM m Unit -> HH.HTML w (HookM m Unit)
toasts toast dismissToast =
    HH.div [ HS.class_ "toasts", HPA.role "status" ] $
    toast # maybe [] \{ text, action } -> pure $
        HH.div [ HS.class_ "toast" ] $
        [ HH.span [ HS.class_ "toast-text" ] [ HH.text text ] ]
        <> maybe []
            (\{ label, onAction } -> [ button Text Small (dismissToast *> onAction) [ HH.text label ] ])
            action
