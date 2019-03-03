module TeamTavern.Infrastructure.Cookie where

import Prelude

import Data.List (List(..), (:))
import Data.List.Types (NonEmptyList(..))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe)
import Data.MultiMap (MultiMap, singleton')
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty ((:|))
import TeamTavern.Player.Domain.Id (Id, fromString, toString)
import TeamTavern.Session.Domain.Token (Token)

type CookieInfo =
    { id :: Id
    , token :: Token
    }

-- Cookie ids.

idCookieName :: String
idCookieName = "teamtavern-id"

tokenCookieName :: String
tokenCookieName = "teamtavern-token"

-- Look up cookies.

lookupCookieInfo :: Map String String -> Maybe CookieInfo
lookupCookieInfo cookies = do
    id <- lookup idCookieName cookies >>= fromString
    token <- lookup tokenCookieName cookies <#> wrap
    pure { id, token }

-- Set cookies.

setIdCookie :: Id -> String
setIdCookie id =
    idCookieName <> "=" <> (toString id)
    <> "; Max-Age=" <> show (top :: Int)
    <> "; Path=/"

setTokenCookie :: Token -> String
setTokenCookie token =
    tokenCookieName <> "=" <> unwrap token
    <> "; Max-Age=" <> show (top :: Int)
    <> "; Path=/"
    <> "; HttpOnly; Secure"

setCookieHeader :: Id -> Token -> MultiMap String String
setCookieHeader id token =
    setIdCookie id :| setTokenCookie token : Nil
    # NonEmptyList
    # singleton' "Set-Cookie"

-- Remove cookies.

removeIdCookie :: String
removeIdCookie =
    idCookieName <> "=; expires=Thu, 01 Jan 1970 00:00:00 GMT; Path=/"

removeTokenCookie :: String
removeTokenCookie =
    tokenCookieName <> "=; expires=Thu, 01 Jan 1970 00:00:00 GMT; Path=/"

removeCookieHeader :: MultiMap String String
removeCookieHeader =
    removeIdCookie :| removeTokenCookie : Nil
    # NonEmptyList
    # singleton' "Set-Cookie"
