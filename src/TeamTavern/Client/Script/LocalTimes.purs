module TeamTavern.Client.Script.LocalTimes (showLocalTimes) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import TeamTavern.Client.Script.Clock (clock)

foreign import showLocalTimesImpl :: (Int -> String) -> Effect Unit

-- | Shows each time of day a guide's text gives in UTC, as a `<time>`, in the
-- | reader's own zone, for today, once the page has drawn it. Each label that
-- | names the zone, `data-local`, then reads as its attribute says, `{zone}`
-- | being the zone's name. A browser that reports UTC, as the prerender's and
-- | privacy modes do, keeps the guide's own text.
showLocalTimes :: ∀ monad. MonadEffect monad => monad Unit
showLocalTimes = liftEffect $ showLocalTimesImpl clock
