module TeamTavern.Infrastructure.Cookie where

import Prelude

import Data.Int (fromString)
import Data.List (List(..), (:))
import Data.List.Types (NonEmptyList(..))
import Data.Map (Map, lookup)
import Data.Maybe (Maybe)
import Data.MultiMap (MultiMap, singleton, singleton')
import Data.Newtype (unwrap)
import Data.NonEmpty ((:|))
import Data.Validated as Validated
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Nickname as Nickname
import TeamTavern.Player.Domain.PlayerId as PlayerId
import TeamTavern.Player.Domain.Token as Token
import TeamTavern.Player.Domain.Types (AuthInfo)

class PlayerId playerId where
    fromPlayerId :: playerId -> Int

class Token token where
    fromToken :: token -> String

idCookieName :: String
idCookieName = "teamtavern-id"

nicknameCookieName :: String
nicknameCookieName = "teamtavern-nickname"

tokenCookieName :: String
tokenCookieName = "teamtavern-token"

lookupIdCookie :: Map String String -> Maybe String
lookupIdCookie = lookup idCookieName

lookupAuthCookies :: Map String String -> Maybe AuthInfo
lookupAuthCookies cookies = do
    id <- lookup idCookieName cookies
        >>= fromString >>= PlayerId.create
    nickname <- lookup nicknameCookieName cookies
        >>= (Nickname.create >>> Validated.hush)
    token <- lookup tokenCookieName cookies
        >>= (Token.create >>> Validated.hush)
    pure { id, nickname, token }

idCookie :: forall playerId. PlayerId playerId => playerId -> String
idCookie id =
    idCookieName <> "=" <> (show $ fromPlayerId id)
    <> "; Max-Age=" <> show (top :: Int)
    <> "; Path=/"

nicknameCookie :: Nickname -> String
nicknameCookie nickname =
    nicknameCookieName <> "=" <> unwrap nickname
    <> "; Max-Age=" <> show (top :: Int)
    <> "; Path=/"

tokenCookie :: forall token. Token token => token -> String
tokenCookie token =
    tokenCookieName <> "=" <> fromToken token
    <> "; Max-Age=" <> show (top :: Int)
    <> "; Path=/"
    <> "; HttpOnly; Secure"

setNicknameCookieHeader :: Nickname -> MultiMap String String
setNicknameCookieHeader nickname =
    singleton "Set-Cookie" $ nicknameCookie nickname

setCookieHeader :: forall playerId token. PlayerId playerId => Token token =>
    playerId -> token -> MultiMap String String
setCookieHeader id token =
    idCookie id :| tokenCookie token : Nil
    # NonEmptyList
    # singleton' "Set-Cookie"

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
