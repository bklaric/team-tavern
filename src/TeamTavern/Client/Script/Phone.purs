module TeamTavern.Client.Script.Phone (isPhone, onPhoneChange) where

import Prelude

import Effect (Effect)

-- Below 640 px the site is laid out for a phone, the one width breakpoint the
-- stylesheets share.
foreign import isPhone :: Effect Boolean

-- Calls back with whether the viewport is a phone's each time that changes;
-- the effect returned stops it.
foreign import onPhoneChange :: (Boolean -> Effect Unit) -> Effect (Effect Unit)
