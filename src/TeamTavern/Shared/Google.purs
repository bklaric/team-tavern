-- | The OAuth client TeamTavern signs players in with Google through. The
-- | browser names it when it sends the player to Google, and the server when it
-- | swaps the code Google sent back.
module TeamTavern.Shared.Google (googleClientId) where

googleClientId :: String
googleClientId = "824524623267-bmst33ajog4dpmcceb4e18o1g5q0cs3p.apps.googleusercontent.com"
