module TeamTavern.Client.Script.Cookie
    ( PlayerInfo
    , hasPlayerIdCookie
    , getPlayerId
    , getNickname
    , getPlayerInfo
    ) where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array ((!!))
import Data.Maybe (Maybe, fromMaybe)
import Data.String (Pattern(..), split, trim)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object (Object, fromFoldable, lookup)
import TeamTavern.Infrastructure.Cookie (idCookieName, nicknameCookieName)

foreign import hasPlayerIdCookie :: Effect Boolean

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

getPlayerId :: Effect (Maybe String)
getPlayerId = getCookie idCookieName

getNickname :: Effect (Maybe String)
getNickname = getCookie nicknameCookieName

type PlayerInfo = { id :: String, nickname :: String }

getPlayerInfo :: Effect (Maybe PlayerInfo)
getPlayerInfo = runMaybeT do
    id <- MaybeT getPlayerId
    nickname <- MaybeT getNickname
    pure { id, nickname }
