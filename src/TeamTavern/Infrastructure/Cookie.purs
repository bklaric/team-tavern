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
import TeamTavern.Player.Domain.PlayerId as PlayerId
import TeamTavern.Player.Domain.Token (Token)

idCookieName :: String
idCookieName = "teamtavern-id"

tokenCookieName :: String
tokenCookieName = "teamtavern-token"

lookupIdCookie :: Map String String -> Maybe String
lookupIdCookie = lookup idCookieName

idCookie :: PlayerId -> String
idCookie id =
    idCookieName <> "=" <> PlayerId.toString id
    <> "; Max-Age=" <> show (top :: Int)
    <> "; Path=/"

tokenCookie :: Token -> String
tokenCookie token =
    tokenCookieName <> "=" <> unwrap token
    <> "; Max-Age=" <> show (top :: Int)
    <> "; Path=/"
    <> "; HttpOnly; Secure"

setCookieHeader :: PlayerId -> Token -> MultiMap String String
setCookieHeader id token =
    idCookie id
    :| (singleton $ tokenCookie token)
    # NonEmptyList
    # singleton' "Set-Cookie"
