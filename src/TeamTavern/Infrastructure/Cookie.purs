module TeamTavern.Infrastructure.Cookie where

import Prelude

import Data.Either as Either
import Data.List (List(..), (:))
import Data.List.Types (NonEmptyList(..))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.MultiMap (MultiMap, singleton')
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Nickname as Nickname
import TeamTavern.Player.Domain.PlayerId (PlayerId)
import TeamTavern.Player.Domain.PlayerId as PlayerId
import TeamTavern.Player.Domain.Token (Token)
import TeamTavern.Player.Domain.Token as Token
import TeamTavern.Player.Domain.Types (NicknamedToken)
import Data.Validated as Validated

idCookieName :: String
idCookieName = "teamtavern-id"

nicknameCookieName :: String
nicknameCookieName = "teamtavern-nickname"

tokenCookieName :: String
tokenCookieName = "teamtavern-token"

lookupIdCookie :: Map String String -> Maybe String
lookupIdCookie = lookup idCookieName

lookupAuthCookies :: Map String String -> Maybe NicknamedToken
lookupAuthCookies cookies = do
    nickname <- lookup nicknameCookieName cookies
        >>= (Nickname.create >>> Validated.hush)
    token <- lookup tokenCookieName cookies
        >>= (Token.create >>> Either.hush)
    pure { nickname, token }

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
