module TeamTavern.Infrastructure.Cookie where

import Prelude

import Data.List (singleton)
import Data.List.Types (NonEmptyList(..))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import MultiMap (MultiMap, singleton')
import TeamTavern.Player.Domain.PlayerId (PlayerId)
import TeamTavern.Player.Domain.Token (Token)

idCookieName :: String
idCookieName = "teamtavern-id"

tokenCookieName :: String
tokenCookieName = "teamtavern-token"

lookupIdCookie :: Map String String -> Maybe String
lookupIdCookie = lookup idCookieName

setCookieValue :: String -> String -> String
setCookieValue key value =
    key <> "=" <> value
    <> "; Max-Age=" <> show (top :: Int)
    <> "; HttpOnly; Secure"

setCookieHeader :: PlayerId -> Token -> MultiMap String String
setCookieHeader id token =
    setCookieValue idCookieName (show id)
    :| (singleton $ setCookieValue tokenCookieName (unwrap token))
    # NonEmptyList
    # singleton' "Set-Cookie"
