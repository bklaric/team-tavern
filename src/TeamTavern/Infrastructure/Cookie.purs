module TeamTavern.Infrastructure.Cookie where

import Prelude

import Data.Map (Map, lookup)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import MultiMap (MultiMap, singleton)
import TeamTavern.Player.Domain.PlayerId (PlayerId)
import TeamTavern.Player.Domain.Token (Token)

tokenCookieName :: String
tokenCookieName = "teamtavern-token"

idCookieName :: String
idCookieName = "teamtavern-id"

lookupTokenCookie :: Map String String -> Maybe String
lookupTokenCookie = lookup tokenCookieName

setCookieValue :: PlayerId -> Token -> String
setCookieValue id token =
    tokenCookieName <> "=" <> unwrap token
    <> "; Max-Age=" <> show (top :: Int)
    <> "; HttpOnly; Secure"

setCookieHeader :: PlayerId -> Token -> MultiMap String String
setCookieHeader id token = singleton "Set-Cookie" (setCookieValue id token)
