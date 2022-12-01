module Jarilo.Fetch where

import Prelude

import Async (Async, attempt, giveUp, left)
import Async as Async
import Browser.Async.Fetch (body, credentials, method)
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response (status, text)
import Browser.Fetch (Credentials)
import Browser.Fetch.Response (status)
import Browser.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.HTTP.Method as Http
import Data.Maybe (Maybe(..), maybe)
import Data.Options ((:=))
import Data.Options as Options
import Data.Show (class ShowRecordFields)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, inj)
import Effect (Effect)
import Effect.Class.Console (log)
import Error (Error)
import Error.Class (message)
import Jarilo.Method (Delete, Get, Head, Method, Options, Patch, Post, Put)
import Jarilo.Path (type (:>), Capture, Literal, Path)
import Jarilo.Query (type (:?), Mandatory, Many, NoQuery, Optional, Query, QueryChain, Rest)
import Jarilo.Response (type (:!), BadRequest, BadRequest_, Internal, Internal_, NoContent, Ok, Ok_, Response)
import Jarilo.Route (FullRoute, Route)
import Jarilo.Shared.Component (class Component, toComponent)
import Prim.Row (class Cons, class Lacks, class Nub, class Union)
import Prim.RowList (class RowToList)
import Record (get, insert, merge)
import Record.Builder (Builder, build)
import Record.Builder as Builder
import Test.QuickCheck (assertEquals)
import Type.Proxy (Proxy(..))
import Undefined (undefined)
import Yoga.JSON (class ReadForeign, class WriteForeign, writeJSON)
import Yoga.JSON.Async as JsonAsync

-- fetch proxy
-- Option.Record
-- ( pathParameters :: { pathParams } -- from proxy
-- , queryParameters :: { queryParams } -- from proxy
-- , body :: { body } -- from proxy
-- )
-- ( host :: String -- e.g. https://www.teamtavern.net
-- , pathPrefix :: Proxy pathPrefix
-- , credentials :: Credentials
-- )

class RequestUrlPath (path :: Path) parameters | path -> parameters where
    requestUrlPath :: Proxy path -> Record parameters -> String

instance IsSymbol name => RequestUrlPath (Literal name) parameters where
    requestUrlPath _ _ = "/" <> reflectSymbol (Proxy :: _ name)

instance (IsSymbol name, Component value, Cons name value parameters' parameters) => RequestUrlPath (Capture name value) parameters where
    requestUrlPath _ parameters = "/" <> ((toComponent :: value -> String) $ get (Proxy :: _ name) parameters)

instance (RequestUrlPath leftPath parameters, RequestUrlPath rightPath parameters) => RequestUrlPath (leftPath :> rightPath) parameters where
    requestUrlPath _ parameters = requestUrlPath (Proxy :: _ leftPath) parameters <> requestUrlPath (Proxy :: _ rightPath) parameters



class RequestUrlQuery (query :: Query) parameters | query -> parameters where
    requestUrlQuery :: Proxy query -> Record parameters -> Maybe String

instance RequestUrlQuery NoQuery parameters where
    requestUrlQuery _ _ = Nothing

instance (IsSymbol name, Component value, Cons name (Maybe value) parameters' parameters) => RequestUrlQuery (Optional name value) parameters where
    requestUrlQuery _ parameters = get (Proxy :: _ name) parameters <#> toComponent <#> \value -> reflectSymbol (Proxy :: _ name) <> "=" <> value

instance (IsSymbol name, Component value, Cons name value parameters' parameters) => RequestUrlQuery (Mandatory name value) parameters where
    requestUrlQuery _ parameters = get (Proxy :: _ name) parameters # toComponent # \value -> Just $ reflectSymbol (Proxy :: _ name) <> "=" <> value

instance (IsSymbol name, Component value, Cons name (Array value) parameters' parameters) => RequestUrlQuery (Many name value) parameters where
    requestUrlQuery _ parameters = get (Proxy :: _ name) parameters <#> toComponent <#> (\value -> reflectSymbol (Proxy :: _ name) <> "=" <> value) # joinWith "&" # Just

instance (IsSymbol name,  Component name', Component value, Cons name (Array (Tuple name' value)) parameters' parameters) => RequestUrlQuery (Rest name) parameters where
    requestUrlQuery _ parameters = get (Proxy :: _ name) parameters <#> (\(Tuple name value) -> toComponent name <> "=" <> toComponent value) # joinWith "&" # Just

instance (RequestUrlQuery leftQuery parameters, RequestUrlQuery rightQuery parameters) => RequestUrlQuery (leftQuery :? rightQuery) parameters where
    requestUrlQuery _ parameters =
        case requestUrlQuery (Proxy :: _ leftQuery) parameters, requestUrlQuery (Proxy :: _ rightQuery) parameters of
        Nothing, Nothing -> Nothing
        Just left, Nothing -> Just left
        Nothing, Just right -> Just right
        Just left, Just right -> Just $ left <> "&" <> right

requestUrlQuery' :: forall parameters query. RequestUrlQuery query parameters => Proxy query -> Record parameters -> String
requestUrlQuery' proxy parameters = maybe "" ("?" <> _) (requestUrlQuery proxy parameters)



requestUrl :: forall pathParameters queryParameters path query.
    RequestUrlPath path pathParameters => RequestUrlQuery query queryParameters =>
    Proxy path -> Proxy query -> Record pathParameters -> Record queryParameters -> String
requestUrl pathProxy queryProxy pathParameters queryParameters = requestUrlPath pathProxy pathParameters <> requestUrlQuery' queryProxy queryParameters



class RequestMethod (method :: Method) body | method -> body where
    requestMethod :: Proxy method -> body -> Options.Options Fetch.FetchOptions

instance RequestMethod Get body where
    requestMethod _ _ = method := Http.GET

instance RequestMethod Head body where
    requestMethod _ _ = method := Http.HEAD

instance RequestMethod Options body where
    requestMethod _ _ = method := Http.OPTIONS

instance RequestMethod Delete body where
    requestMethod _ _ = method := Http.DELETE

instance WriteForeign body => RequestMethod (Post body) body where
    requestMethod _ body' = method := Http.POST <> body := writeJSON body'

instance WriteForeign body => RequestMethod (Put body) body where
    requestMethod _ body' = method := Http.PUT <> body := writeJSON body'

instance WriteForeign body => RequestMethod (Patch body) body where
    requestMethod _ body' = method := Http.PATCH <> body := writeJSON body'



class ReadResponse (response :: Response) start end result | response -> start end result where
    readResponse
        :: Proxy response
        -> FetchRes.Response
        -> Async (Builder (Record start) (Record end)) (Variant result)

instance (ReadForeign body, Lacks "ok" start) => ReadResponse (Ok body) start (ok :: String | start) (ok :: body | result) where
    readResponse _ response =
        case status response of
        200 -> do
            jsonBody <- response # text >>= JsonAsync.readJSON # lmap (\error -> Builder.insert (Proxy :: _ "ok") (show error))
            pure $ inj (Proxy :: Proxy "ok") jsonBody
        status' -> Async.left $ Builder.insert (Proxy :: _ "ok") $ "Wrong status " <> show status'

instance (ReadForeign body, Lacks "badRequest" start) => ReadResponse (BadRequest body) start (badRequest :: String | start) (badRequest :: body | result) where
    readResponse _ response =
        case status response of
        400 -> do
            jsonBody <- response # text >>= JsonAsync.readJSON # lmap (\error -> Builder.insert (Proxy :: _ "badRequest") (show error))
            pure $ inj (Proxy :: Proxy "badRequest") jsonBody
        status' -> Async.left $ Builder.insert (Proxy :: _ "badRequest") $ "Wrong status " <> show status'

instance (ReadForeign body, Lacks "internal" start) => ReadResponse (Internal body) start (internal :: String | start) (internal :: body | result) where
    readResponse _ response =
        case status response of
        500 -> do
            jsonBody <- response # text >>= JsonAsync.readJSON # lmap (\error -> Builder.insert (Proxy :: _ "internal") (show error))
            pure $ inj (Proxy :: Proxy "internal") jsonBody
        status' -> Async.left $ Builder.insert (Proxy :: _ "internal") $ "Wrong status " <> show status'

instance (Lacks "ok" start) => ReadResponse Ok_ start (ok :: String | start) (ok :: Unit | result) where
    readResponse _ response =
        case status response of
        200 -> pure $ inj (Proxy :: Proxy "ok") unit
        status' -> Async.left $ Builder.insert (Proxy :: _ "ok") $ "Wrong status " <> show status'

instance (Lacks "noContent" start) => ReadResponse NoContent start (noContent :: String | start) (noContent :: Unit | result) where
    readResponse _ response =
        case status response of
        204 -> pure $ inj (Proxy :: Proxy "noContent") unit
        status' -> Async.left $ Builder.insert (Proxy :: _ "noContent") $ "Wrong status " <> show status'

instance (Lacks "badRequest" start) => ReadResponse BadRequest_ start (badRequest :: String | start) (badRequest :: Unit | result) where
    readResponse _ response =
        case status response of
        400 -> pure $ inj (Proxy :: Proxy "badRequest") unit
        status' -> Async.left $ Builder.insert (Proxy :: _ "badRequest") $ "Wrong status " <> show status'

instance (Lacks "internal" start) => ReadResponse Internal_ start (internal :: String | start) (internal :: Unit | result) where
    readResponse _ response =
        case status response of
        500 -> pure $ inj (Proxy :: Proxy "internal") unit
        status' -> Async.left $ Builder.insert (Proxy :: _ "internal") $ "Wrong status " <> show status'

instance (ReadResponse leftResponse start mid result, ReadResponse rightResponse mid end result) => ReadResponse (leftResponse :! rightResponse) start end result where
    readResponse _ response = let
        leftProxy = (Proxy :: _ leftResponse)
        rightProxy = (Proxy :: _ rightResponse)
        in
        readResponse leftProxy response # attempt >>= (case _ of
            Left leftError -> readResponse rightProxy response # attempt <#> case _ of
                Left rightError -> Left $ leftError >>> rightError
                Right rightResult -> Right rightResult
            Right leftResult -> pure $ Right leftResult)
        # giveUp

readResponse' :: forall response result errors errorsList.
    ReadResponse response () errors result => Nub errors errors => RowToList errors errorsList => ShowRecordFields errorsList errors =>
    Proxy response -> FetchRes.Response -> Async String (Variant result)
readResponse' proxy response = readResponse proxy response # lmap (flip build {} >>> show)

a ∷ ∀ (r11077 ∷ Row Type). Variant ( internal ∷ Unit | r11077 )
a = inj (Proxy :: Proxy "internal") unit


type FetchOptions = { origin :: Maybe String, pathPrefix :: Maybe String, credentials :: Maybe Credentials }

defaultOptions :: FetchOptions
defaultOptions = { origin: Nothing, pathPrefix: Nothing, credentials: Nothing }



class Fetch (route :: Route) pathParameters queryParameters body responses | route -> pathParameters queryParameters body responses where
    fetch :: Proxy route -> { pathParameters :: Record pathParameters, queryParameters :: Record queryParameters, body :: body } -> FetchOptions -> Async String (Variant responses)

instance (RequestMethod method body, RequestUrlPath path pathParameters, RequestUrlQuery query queryParameters, ReadResponse response () errors responses, Nub errors errors, RowToList errors errorsList, ShowRecordFields errorsList errors) => Fetch (FullRoute method path query response) pathParameters queryParameters body responses where
    fetch _ { pathParameters, queryParameters, body } options = let
        origin = maybe "" identity options.origin
        pathPrefix = maybe "" identity options.pathPrefix
        url = origin <> pathPrefix <> requestUrl (Proxy :: _ path) (Proxy :: _ query) pathParameters queryParameters
        fetchOptions = requestMethod (Proxy :: _ method) body <> maybe mempty (credentials := _) options.credentials
        in
        Fetch.fetch url fetchOptions # lmap message >>= readResponse' (Proxy :: _ response)
