module TeamTavern.Infrastructure.Cookie where

import Data.Map (Map, lookup)
import Data.Maybe (Maybe)

cookieName :: String
cookieName = "teamtavern-token"

lookupCookie :: Map String String -> Maybe String
lookupCookie = lookup cookieName
