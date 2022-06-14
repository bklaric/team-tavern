module Perun.Request where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array (mapMaybe)
import Data.Bifunctor (lmap)
import Data.Either (Either, hush)
import Data.Either.Swap (swap)
import Data.HTTP.Method (CustomMethod, Method, fromString)
import Data.Map (Map, empty, fromFoldable)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Foreign (Foreign, readString)
import Foreign.Object (lookup, toUnfoldable)
import Node.Http.IncomingMessage (headers)
import Node.Http.Server.Request (method, url)
import Node.Http.Server.Request as Node
import Perun.Request.Body (Body, fromRequest)
import Perun.Url (Url, parseUrl)

type Request =
    { method :: Either CustomMethod Method
    , url :: Either String Url
    , headers :: Map String String
    , cookies :: Map String String
    , body :: Body
    }

readMethod :: Node.Request -> Either CustomMethod Method
readMethod request = request # method # fromString # swap

readUrl :: Node.Request -> Either String Url
readUrl request = request # url # parseUrl # lmap (const $ url request)

readString' ∷ Foreign → Maybe String
readString' = readString >>> runExcept >>> hush

readHeaders :: Node.Request -> Map String String
readHeaders request =
    headers request
    # toUnfoldable
    # mapMaybe (traverse readString')
    # fromFoldable

parseCookies :: String -> Array (Tuple String String)
parseCookies string =
    split (Pattern "; ") string
    <#> split (Pattern "=")
    # mapMaybe case _ of
        [name, value] -> Just $ Tuple name value
        _ -> Nothing

readCookies :: Node.Request -> Map String String
readCookies request =
    headers request
    # lookup "cookie"
    >>= readString'
    # maybe empty (parseCookies >>> fromFoldable)

readRequest :: Node.Request -> Request
readRequest request =
    { method: readMethod request
    , url: readUrl request
    , headers: readHeaders request
    , cookies: readCookies request
    , body: fromRequest request
    }
