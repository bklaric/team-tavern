module TeamTavern.Server.Infrastructure.RequestOrigin (requestOrigin) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)

-- | The origin a request came in on, for the files whose addresses are
-- | absolute: each origin the site answers on (production, staging, the local
-- | stacks) lists its own. Caddy passes the request's host on and says in
-- | `X-Forwarded-Proto` how it was reached.
requestOrigin :: Map String String -> String
requestOrigin headers =
    (Map.lookup "x-forwarded-proto" headers # fromMaybe "http")
    <> "://"
    <> (Map.lookup "host" headers # fromMaybe "")
