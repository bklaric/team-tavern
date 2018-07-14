module TeamTavern.Infrastructure.Cookie where

import Prelude

import Data.Map (Map, lookup)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import MultiMap (MultiMap, singleton)
import TeamTavern.Player.Token (Token)

cookieName :: String
cookieName = "teamtavern-token"

lookupCookie :: Map String String -> Maybe String
lookupCookie = lookup cookieName

setCookieValue :: Token -> String
setCookieValue token =
    cookieName <> "=" <> unwrap token
    <> "; Max-Age=" <> show (top :: Int)
    <> "; HttpOnly; Secure"

setCookieHeader :: Token -> MultiMap String String
setCookieHeader token = singleton "Set-Cookie" (setCookieValue token)
