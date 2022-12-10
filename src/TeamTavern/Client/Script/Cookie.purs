module TeamTavern.Client.Script.Cookie
    ( PlayerId
    , Nickname
    , PlayerInfo(..)
    , getPlayerId
    , getPlayerNickname
    , getPlayerInfo
    , hasPlayerIdCookie
    ) where

import Prelude

import Control.Bind (bindFlipped)
import Data.Array ((!!))
import Data.Int (fromString)
import Data.Maybe (Maybe, fromMaybe, isJust)
import Data.String (Pattern(..), split, trim)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Foreign.Object (Object, fromFoldable, lookup)
import TeamTavern.Server.Infrastructure.Cookie (idCookieName, nicknameCookieName)

type PlayerId = Int

type Nickname = String

type PlayerInfo = { id :: PlayerId, nickname :: Nickname }

foreign import cookies :: Effect String

toTuple :: String -> Tuple String String
toTuple cookie = let
    keyValue = split (Pattern "=") cookie
    key = fromMaybe "" $ keyValue !! 0
    value = fromMaybe "" $ keyValue !! 1
    in
    Tuple key value

parseCookies :: String -> Object String
parseCookies unparsedCookies =
    unparsedCookies
    # split (Pattern ";")
    <#> (trim >>> toTuple)
    # fromFoldable

getCookies :: ∀ effect. MonadEffect effect => effect (Object String)
getCookies = cookies <#> parseCookies # liftEffect

getCookie :: ∀ effect. MonadEffect effect => String -> effect (Maybe String)
getCookie key = getCookies <#> lookup key

getPlayerId :: ∀ effect. MonadEffect effect => effect (Maybe Int)
getPlayerId = getCookie idCookieName <#> bindFlipped fromString

getPlayerNickname :: ∀ effect. MonadEffect effect => effect (Maybe String)
getPlayerNickname = getCookie nicknameCookieName

getPlayerInfo :: ∀ effect. MonadEffect effect => effect (Maybe PlayerInfo)
getPlayerInfo = do
    id <- getPlayerId
    nickname <- getPlayerNickname
    pure $ { id: _, nickname: _ } <$> id <*> nickname

hasPlayerIdCookie :: ∀ effect. MonadEffect effect => effect Boolean
hasPlayerIdCookie = getPlayerId <#> isJust
