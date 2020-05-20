module TeamTavern.Client.Script.Timezone where

import Effect (Effect)

foreign import getClientTimezone :: Effect String
