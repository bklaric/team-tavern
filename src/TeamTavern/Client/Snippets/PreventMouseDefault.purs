module TeamTavern.Client.Snippets.PreventMouseDefault where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

preventMouseDefault :: ∀ monad. MonadEffect monad => MouseEvent -> monad Unit
preventMouseDefault = liftEffect <<< preventDefault <<< toEvent
