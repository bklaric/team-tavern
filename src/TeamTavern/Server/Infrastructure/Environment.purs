module TeamTavern.Server.Infrastructure.Environment where

import Data.Maybe (Maybe(..))

-- | Which stack the server runs in, named by ENVIRONMENT. bundle.ps1 builds for
-- | the same names and gives each build its environment's Caddyfile.
data Environment = Local | Staging | Production

fromString :: String -> Maybe Environment
fromString = case _ of
    "local" -> Just Local
    "staging" -> Just Staging
    "production" -> Just Production
    _ -> Nothing

-- | The local stack serves plain HTTP on localhost; the others serve HTTPS.
servesHttps :: Environment -> Boolean
servesHttps = case _ of
    Local -> false
    Staging -> true
    Production -> true
