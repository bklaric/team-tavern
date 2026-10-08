module TeamTavern.Client.Script.Timezone (getClientTimezone, getListedTimezone) where

import Prelude

import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect, liftEffect)
import JavaScript.Intl.DateTimeFormat as DateTimeFormat
import TeamTavern.Shared.Timezones (listedTimezone)

-- | The browser's zone by the name it gives it, the one name its own Intl is
-- | sure to know, which times are shown in. The list may have the zone under a
-- | name the browser is too old to know (Europe/Kyiv for its Europe/Kiev).
getClientTimezone :: ∀ effect. MonadEffect effect => effect String
getClientTimezone = liftEffect $ DateTimeFormat.new__ <#> DateTimeFormat.resolvedOptions <#> _.timeZone

-- | The browser's zone as the list names it, which a form starts from, or
-- | nothing where the list doesn't have it, as with the UTC that privacy modes
-- | report.
getListedTimezone :: ∀ effect. MonadEffect effect => effect (Maybe String)
getListedTimezone = getClientTimezone <#> listedTimezone
