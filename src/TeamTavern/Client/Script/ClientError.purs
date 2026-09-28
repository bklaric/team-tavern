module TeamTavern.Client.Script.ClientError (bundleName, onOwnError) where

import Prelude

import Effect (Effect)

-- | The file name of the site's script, hashed by build, so a report says
-- | which build the page runs. Empty where the script isn't the site's bundle.
foreign import bundleName :: String

-- | Calls back with the message of every error the site's own script throws
-- | and nothing catches. Those of extensions and the ad script are left out,
-- | as nothing here can fix them.
foreign import onOwnError :: (String -> Effect Unit) -> Effect Unit
