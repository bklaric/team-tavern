module Perun.Request where

import Prelude

import Async (Async)
import Control.Monad.Except (runExcept)
import Data.Array (mapMaybe)
import Data.Bifunctor (lmap)
import Data.Either (Either, hush)
import Data.Either.Swap (swap)
import Data.HTTP.Method (CustomMethod, Method, fromString)
import Data.List (List)
import Data.Map (Map, empty, fromFoldable)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign (Foreign, readString)
import Foreign.Object (lookup, toUnfoldable)
import Node.Http.IncomingMessage (headers)
import Node.Http.Server.Request (method, url)
import Node.Http.Server.Request as Node
import Partial.Unsafe (unsafePartial)
import Perun.Async.Request.Body (readBody)
import Perun.Request.Body (fromRequest, readAsUtf8)
import Perun.Url (Url, parseUrl, pathSegments, queryPairs)
import URI.Extra.QueryPairs (Key, QueryPairs, Value)
import URI.Path.Segment (PathSegment)

type Request =
    { method :: Either CustomMethod Method
    , path :: List PathSegment
    , query :: QueryPairs Key Value
    , headers :: Map String String
    , cookies :: Map String String
    , body :: String
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

-- readRequest :: forall left. Node.Request -> Async left Request
-- readRequest :: Request -> (Node.Request
--     -> Effect Unit
--    )
--    -> Effect Unit
readRequest :: Node.Request -> ({ body :: String
    , cookies :: Map String String
    , headers :: Map String String
    , method :: Either CustomMethod Method
    , path :: List PathSegment
    , query :: QueryPairs Key Value
    }
    -> Effect Unit
   )
   -> Effect Unit
readRequest request callback =
    (request # fromRequest # readAsUtf8 \body -> let
        url' = unsafePartial (readUrl request # hush # fromJust)
        in
        callback
            { method: readMethod request
            , path: pathSegments url'
            , query: queryPairs url'
            , headers: readHeaders request
            , cookies: readCookies request
            , body
            }) # void
