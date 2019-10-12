module TeamTavern.Client.Script.Cookie
    ( PlayerId
    , Nickname
    , PlayerInfo(..)
    , getPlayerId
    , getPlayerNickname
    , getPlayerInfo
    , playerHasId
    , playerHasNickame
    , hasPlayerIdCookie
    ) where

import Prelude

import Control.Bind (bindFlipped)
import Data.Array ((!!))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String (Pattern(..), split, trim)
import Data.Tuple (Tuple(..))
import Effect (Effect)
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

getCookies :: Effect (Object String)
getCookies = cookies <#> parseCookies

getCookie :: String -> Effect (Maybe String)
getCookie key = getCookies <#> lookup key

getPlayerId :: Effect (Maybe Int)
getPlayerId = getCookie idCookieName <#> bindFlipped fromString

getPlayerNickname :: Effect (Maybe String)
getPlayerNickname = getCookie nicknameCookieName

getPlayerInfo :: Effect (Maybe PlayerInfo)
getPlayerInfo = do
    id' <- getPlayerId
    nickname' <- getPlayerNickname
    pure $ case Tuple id' nickname' of
        Tuple (Just id) (Just nickname) -> Just $ { id, nickname }
        _ -> Nothing

playerHasId :: Int -> Effect Boolean
playerHasId id = getPlayerId <#> maybe false (_ == id)

playerHasNickame :: String -> Effect Boolean
playerHasNickame nickname = getPlayerNickname <#> maybe false (_ == nickname)

hasPlayerIdCookie :: Effect Boolean
hasPlayerIdCookie = getPlayerId <#> isJust
