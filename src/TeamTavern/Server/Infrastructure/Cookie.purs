module TeamTavern.Server.Infrastructure.Cookie where

import Prelude

import Data.List (List(..), (:))
import Data.List.Types (NonEmptyList(..))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe)
import Data.MultiMap (MultiMap, singleton)
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty ((:|))
import TeamTavern.Server.Player.Domain.Id (Id, fromString, toString)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Session.Domain.Token (Token)

type CookieInfo =
    { id :: Id
    , nickname :: Nickname
    , token :: Token
    }

-- Cookie ids.

idCookieName :: String
idCookieName = "teamtavern-id"

nicknameCookieName :: String
nicknameCookieName = "teamtavern-nickname"

tokenCookieName :: String
tokenCookieName = "teamtavern-token"

-- Look up cookies.

lookupCookieInfo :: Map String String -> Maybe CookieInfo
lookupCookieInfo cookies = do
    id <- lookup idCookieName cookies >>= fromString
    nickname <- lookup nicknameCookieName cookies <#> wrap
    token <- lookup tokenCookieName cookies <#> wrap
    pure { id, nickname, token }

-- Set cookies.

setIdCookie :: Id -> String
setIdCookie id =
    idCookieName <> "=" <> toString id
    <> "; Max-Age=" <> show (top :: Int)
    <> "; Path=/"

setNicknameCookie :: Nickname -> String
setNicknameCookie nickname =
    nicknameCookieName <> "=" <> unwrap nickname
    <> "; Max-Age=" <> show (top :: Int)
    <> "; Path=/"

setTokenCookie :: Token -> String
setTokenCookie token =
    tokenCookieName <> "=" <> unwrap token
    <> "; Max-Age=" <> show (top :: Int)
    <> "; Path=/"
    <> "; HttpOnly; Secure"

setCookieHeader :: CookieInfo -> MultiMap String String
setCookieHeader { id, nickname, token } =
    setIdCookie id :| setNicknameCookie nickname : setTokenCookie token : Nil
    # NonEmptyList
    # singleton "Set-Cookie"

-- Remove cookies.

removeIdCookie :: String
removeIdCookie =
    idCookieName <> "=; expires=Thu, 01 Jan 1970 00:00:00 GMT; Path=/"

removeNicknameCookie :: String
removeNicknameCookie =
    nicknameCookieName <> "=; expires=Thu, 01 Jan 1970 00:00:00 GMT; Path=/"

removeTokenCookie :: String
removeTokenCookie =
    tokenCookieName <> "=; expires=Thu, 01 Jan 1970 00:00:00 GMT; Path=/"

removeCookieHeader :: MultiMap String String
removeCookieHeader =
    removeIdCookie :| removeNicknameCookie : removeTokenCookie : Nil
    # NonEmptyList
    # singleton "Set-Cookie"
