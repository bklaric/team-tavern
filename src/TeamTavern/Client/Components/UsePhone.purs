module TeamTavern.Client.Components.UsePhone (UsePhone, usePhone) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Hooks (type (<>), Hook, HookType, Pure, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Hook (class HookNewtype)
import Halogen.Subscription as Subscription
import TeamTavern.Client.Script.Phone (isPhone, onPhoneChange)

foreign import data UsePhone :: HookType

instance HookNewtype UsePhone (UseState Boolean <> UseEffect <> Pure)

-- Whether the viewport is a phone's, read on mount and again each time it
-- crosses the breakpoint.
usePhone :: ∀ m. MonadEffect m => Hook m UsePhone Boolean
usePhone = Hooks.wrap Hooks.do
    phone /\ phoneId <- Hooks.useState false

    Hooks.useLifecycleEffect do
        liftEffect isPhone >>= Hooks.put phoneId
        subscription <- Hooks.subscribe $ Subscription.makeEmitter onPhoneChange <#> Hooks.put phoneId
        pure $ Just $ Hooks.unsubscribe subscription

    Hooks.pure phone
