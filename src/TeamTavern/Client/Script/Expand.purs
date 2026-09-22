module TeamTavern.Client.Script.Expand (toggleCard) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Web.UIEvent.MouseEvent (MouseEvent)

foreign import data Card :: Type

foreign import cardOf :: MouseEvent -> Effect Card

foreign import heightOf :: Card -> Effect Number

foreign import animateFrom :: Card -> Number -> Effect Unit

-- | Runs the change that expands or collapses the card whose button was
-- | clicked, and grows the card from its old height to its new one, or shrinks
-- | it back. Under reduced motion it only changes.
toggleCard :: ∀ m. MonadEffect m => MouseEvent -> m Unit -> m Unit
toggleCard event change = do
    card <- liftEffect $ cardOf event
    from <- liftEffect $ heightOf card
    change
    liftEffect $ animateFrom card from
