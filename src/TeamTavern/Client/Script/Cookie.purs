module TeamTavern.Client.Script.Cookie
    ( getPlayerId
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
import Foreign.Object (Object, fromFoldable, lookup)
import TeamTavern.Infrastructure.Cookie (idCookieName)

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

hasPlayerIdCookie :: Effect Boolean
hasPlayerIdCookie = getPlayerId <#> isJust
