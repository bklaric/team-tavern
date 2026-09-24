module TeamTavern.Client.Script.Timezone (getClientTimezone) where

import Prelude

import Data.Array (find)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import TeamTavern.Shared.Timezones (allTimezones)

foreign import browserTimezone :: Effect String

-- | The name Intl gives a zone, whichever of its names it is asked with: the
-- | one the browser reports its own zone by.
foreign import intlName :: String -> String

-- | The browser's zone as the site's list names it. Browsers report some zones
-- | by their old names (Asia/Calcutta, Europe/Kiev), which Postgres doesn't
-- | know, so the zone is the list's entry that Intl takes for the same one.
getClientTimezone :: ∀ effect. MonadEffect effect => effect String
getClientTimezone = liftEffect do
    browser <- browserTimezone
    let names = allTimezones <#> _.name
    pure case find (_ == browser) names of
        Just name -> name
        Nothing -> names # find (intlName >>> (_ == browser)) # fromMaybe browser
