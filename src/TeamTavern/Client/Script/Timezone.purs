module TeamTavern.Client.Script.Timezone (getClientTimezone) where

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

foreign import getClientTimezoneImpl :: Effect String

getClientTimezone :: ∀ effect. MonadEffect effect => effect String
getClientTimezone = liftEffect getClientTimezoneImpl
