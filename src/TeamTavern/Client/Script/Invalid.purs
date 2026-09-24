module TeamTavern.Client.Script.Invalid (describeInvalid) where

import Prelude

import Effect (Effect)

-- | From now on, a field's error describes its control and marks it invalid
-- | while it shows (`Components/Field.purs`).
foreign import describeInvalid :: Effect Unit
