module TeamTavern.Client.Script.Consent (showConsentSettings) where

import Prelude

import Effect (Effect)

-- | Opens Google's consent dialog again, where a visitor changes or withdraws
-- | the consent they gave for ads.
foreign import showConsentSettings :: Effect Unit
