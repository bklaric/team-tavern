module TeamTavern.Infrastructure.Cookie where

import Prelude

import Data.List (List(..), (:))
import Data.List.Types (NonEmptyList(..))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import MultiMap (MultiMap, singleton')
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.PlayerId (PlayerId)
import TeamTavern.Player.Domain.PlayerId as PlayerId
import TeamTavern.Player.Domain.Token (Token)

idCookieName :: String
idCookieName = "teamtavern-id"

nicknameCookieName :: String
nicknameCookieName = "teamtavern-nickname"

tokenCookieName :: String
tokenCookieName = "teamtavern-token"

lookupIdCookie :: Map String String -> Maybe String
lookupIdCookie = lookup idCookieName

idCookie :: PlayerId -> String
idCookie id =
    idCookieName <> "=" <> PlayerId.toString id
    <> "; Max-Age=" <> show (top :: Int)
    <> "; Path=/"

nicknameCookie :: Nickname -> String
nicknameCookie nickname =
    nicknameCookieName <> "=" <> unwrap nickname
    <> "; Max-Age=" <> show (top :: Int)
    <> "; Path=/"

tokenCookie :: Token -> String
tokenCookie token =
    tokenCookieName <> "=" <> unwrap token
    <> "; Max-Age=" <> show (top :: Int)
    <> "; Path=/"
    <> "; HttpOnly; Secure"

setCookieHeader :: PlayerId -> Nickname -> Token -> MultiMap String String
setCookieHeader id nickname token =
    idCookie id
    :| (nicknameCookie nickname : tokenCookie token : Nil)
    # NonEmptyList
    # singleton' "Set-Cookie"
