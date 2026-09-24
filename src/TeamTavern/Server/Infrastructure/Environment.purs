module TeamTavern.Server.Infrastructure.Environment where

import Data.Maybe (Maybe(..))

-- | Which stack the server runs in, named by ENVIRONMENT. Compose reads the same
-- | variable to pick the stack's Caddyfile, so the names are those files' names.
data Environment = Development | Test | Staging | Production

fromString :: String -> Maybe Environment
fromString = case _ of
    "development" -> Just Development
    "test" -> Just Test
    "staging" -> Just Staging
    "production" -> Just Production
    _ -> Nothing

-- | The local stacks serve plain HTTP on localhost; the others serve HTTPS.
servesHttps :: Environment -> Boolean
servesHttps = case _ of
    Development -> false
    Test -> false
    Staging -> true
    Production -> true
