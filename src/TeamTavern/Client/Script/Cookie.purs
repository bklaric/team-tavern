module TeamTavern.Client.Script.Cookie
    ( getPlayerId
    , getPlayerNickname
    , playerHasId
    , playerHasNickame
    , hasPlayerIdCookie
    ) where

import Prelude

import Control.Bind (bindFlipped)
import Data.Array ((!!))
import Data.Int (fromString)
import Data.Maybe (Maybe, fromMaybe, isJust, maybe)
import Data.String (Pattern(..), split, trim)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object (Object, fromFoldable, lookup)
import TeamTavern.Infrastructure.Cookie (idCookieName, nicknameCookieName)

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

playerHasId :: Int -> Effect Boolean
playerHasId id = getPlayerId <#> maybe false (_ == id)

playerHasNickame :: String -> Effect Boolean
playerHasNickame nickname = getPlayerNickname <#> maybe false (_ == nickname)

hasPlayerIdCookie :: Effect Boolean
hasPlayerIdCookie = getPlayerId <#> isJust
