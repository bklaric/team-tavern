module TeamTavern.Server.Infrastructure.Cookie
    ( Cookies
    , lookupToken
    , removeCookieHeader
    , setCookieHeader
    ) where

import Prelude

import Data.List (List(..))
import Data.List.Types (NonEmptyList(..))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe)
import Data.MultiMap (MultiMap, singleton)
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty ((:|))
import TeamTavern.Server.Infrastructure.Deployment (Deployment(..))
import TeamTavern.Server.Session.Domain.Token (Token)

-- The session is the one cookie, which only the server reads. The client
-- learns who is signed in by asking the server.

type Cookies = Map String String

tokenCookieName :: String
tokenCookieName = "teamtavern-token"

lookupToken :: Cookies -> Maybe Token
lookupToken cookies = lookup tokenCookieName cookies <#> wrap

-- `SameSite=Lax` keeps a browser from sending the cookie with any request
-- another site starts other than following a link here, so another site can't
-- act as the player.
setCookieHeader :: Deployment -> Token -> MultiMap String String
setCookieHeader deployment token =
    tokenCookieName <> "=" <> unwrap token
    <> "; Max-Age=" <> show (top :: Int)
    <> "; Path=/"
    <> "; SameSite=Lax"
    <> "; HttpOnly"
    <> case deployment of
        Local -> ""
        Cloud -> "; Secure"
    # oneCookie

removeCookieHeader :: MultiMap String String
removeCookieHeader =
    tokenCookieName <> "=; expires=Thu, 01 Jan 1970 00:00:00 GMT; Path=/"
    # oneCookie

oneCookie :: String -> MultiMap String String
oneCookie cookie = singleton "Set-Cookie" $ NonEmptyList $ cookie :| Nil
