module Jarilo.Fetch (FetchOptions, defaultOptions, class Fetch, fetch) where

import Prelude

import Async (Async, attempt, giveUp, left)
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response (text)
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
import Error.Class (message)
import Jarilo.Shared.Component (class Component, toComponent)
import Jarilo.Types (BadRequest, Body, Capture, Delete, Forbidden, FullRequest, FullResponse, FullRoute, Get, Head, Internal, JsonBody, Literal, Mandatory, Many, Method, NoBody, NoContent, NoQuery, NotAuthorized, Ok, Optional, Options, Patch, Path, PathChain, Post, Put, Query, QueryChain, Response, ResponseChain, Rest, Route, Status)
import Prim.Row (class Cons, class Lacks, class Nub)
import Prim.RowList (class RowToList)
import Record (get)
import Record.Builder (Builder, buildFromScratch)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Yoga.JSON (class ReadForeign, class WriteForeign, writeJSON)
import Yoga.JSON.Async as JsonAsync



class FetchMethod (method :: Method) where
    fetchMethod :: Proxy method -> Options.Options Fetch.FetchOptions

instance FetchMethod Options where
    fetchMethod _ = Fetch.method := Http.OPTIONS

instance FetchMethod Head where
    fetchMethod _ = Fetch.method := Http.HEAD

instance FetchMethod Get where
    fetchMethod _ = Fetch.method := Http.GET

instance FetchMethod Post where
    fetchMethod _ = Fetch.method := Http.POST

instance FetchMethod Put where
    fetchMethod _ = Fetch.method := Http.PUT

instance FetchMethod Patch where
    fetchMethod _ = Fetch.method := Http.PATCH

instance FetchMethod Delete where
    fetchMethod _ = Fetch.method := Http.DELETE



class FetchPath (path :: Path) parameters | path -> parameters where
    fetchPath :: Proxy path -> Record parameters -> String

instance (IsSymbol name) =>
    FetchPath (Literal name) parameters where
    fetchPath _ _ = "/" <> reflectSymbol (Proxy :: _ name)

instance (IsSymbol name, Component value, Cons name value parameters' parameters) =>
    FetchPath (Capture name value) parameters where
    fetchPath _ parameters = "/" <> ((toComponent :: value -> String) $ get (Proxy :: _ name) parameters)

instance (FetchPath leftPath parameters, FetchPath rightPath parameters) =>
    FetchPath (PathChain leftPath rightPath) parameters where
    fetchPath _ parameters = fetchPath (Proxy :: _ leftPath) parameters <> fetchPath (Proxy :: _ rightPath) parameters



class FetchQuery (query :: Query) parameters | query -> parameters where
    fetchQuery' :: Proxy query -> Record parameters -> Maybe String

instance FetchQuery NoQuery parameters where
    fetchQuery' _ _ = Nothing

instance (IsSymbol name, Component value, Cons name (Maybe value) parameters' parameters) =>
    FetchQuery (Optional name value) parameters where
    fetchQuery' _ parameters = get (Proxy :: _ name) parameters <#> toComponent <#> \value -> reflectSymbol (Proxy :: _ name) <> "=" <> value

instance (IsSymbol name, Component value, Cons name value parameters' parameters) =>
    FetchQuery (Mandatory name value) parameters where
    fetchQuery' _ parameters = get (Proxy :: _ name) parameters # toComponent # \value -> Just $ reflectSymbol (Proxy :: _ name) <> "=" <> value

instance (IsSymbol name, Component value, Cons name (Array value) parameters' parameters) =>
    FetchQuery (Many name value) parameters where
    fetchQuery' _ parameters = get (Proxy :: _ name) parameters <#> toComponent <#> (\value -> reflectSymbol (Proxy :: _ name) <> "=" <> value) # joinWith "&" # Just

instance (IsSymbol name,  Component name', Component value, Cons name (Array (Tuple name' value)) parameters' parameters) =>
    FetchQuery (Rest name) parameters where
    fetchQuery' _ parameters = get (Proxy :: _ name) parameters <#> (\(Tuple name value) -> toComponent name <> "=" <> toComponent value) # joinWith "&" # Just

instance (FetchQuery leftQuery parameters, FetchQuery rightQuery parameters) =>
    FetchQuery (QueryChain leftQuery rightQuery) parameters where
    fetchQuery' _ parameters =
        case fetchQuery' (Proxy :: _ leftQuery) parameters, fetchQuery' (Proxy :: _ rightQuery) parameters of
        Nothing, Nothing -> Nothing
        Just left, Nothing -> Just left
        Nothing, Just right -> Just right
        Just left, Just right -> Just $ left <> "&" <> right

fetchQuery :: forall parameters query. FetchQuery query parameters => Proxy query -> Record parameters -> String
fetchQuery proxy parameters = maybe "" ("?" <> _) (fetchQuery' proxy parameters)



fetchUrl :: forall pathParameters queryParameters path query.
    FetchPath path pathParameters => FetchQuery query queryParameters =>
    Maybe String -> Maybe String -> Proxy path -> Proxy query -> Record pathParameters -> Record queryParameters -> String
fetchUrl origin' pathPrefix' pathProxy queryProxy pathParameters queryParameters = let
        origin = maybe "" identity origin'
        pathPrefix = maybe "" identity pathPrefix'
        path = fetchPath pathProxy pathParameters
        query = fetchQuery queryProxy queryParameters
        in
        origin <> pathPrefix <> path <> query



class FetchBody (body :: Body) realBody | body -> realBody where
    fetchRequestBody :: Proxy body -> realBody -> Options.Options Fetch.FetchOptions
    fetchResponseBody :: Proxy body -> FetchRes.Response -> Async String realBody

instance FetchBody NoBody Unit where
    fetchRequestBody _ _ = mempty
    fetchResponseBody _ _ = pure unit

instance (WriteForeign realBody, ReadForeign realBody) => FetchBody (JsonBody realBody) realBody where
    fetchRequestBody _ realBody = Fetch.body := writeJSON realBody
    fetchResponseBody _ response = response # text >>= JsonAsync.readJSON # lmap show



class FetchStatusResult (status :: Status) realBody results | status -> realBody results where
    createResult :: Proxy status -> (realBody -> Variant results)

instance FetchStatusResult Ok realBody (ok :: realBody | results) where
    createResult _ = inj (Proxy :: _ "ok")

instance FetchStatusResult NoContent realBody (noContent :: realBody | results) where
    createResult _ = inj (Proxy :: _ "noContent")

instance FetchStatusResult BadRequest realBody (badRequest :: realBody | results) where
    createResult _ = inj (Proxy :: _ "badRequest")

instance FetchStatusResult NotAuthorized realBody (notAuthorized :: realBody | results) where
    createResult _ = inj (Proxy :: _ "notAuthorized")

instance FetchStatusResult Forbidden realBody (forbidden :: realBody | results) where
    createResult _ = inj (Proxy :: _ "forbidden")

instance FetchStatusResult Internal realBody (internal :: realBody | results) where
    createResult _ = inj (Proxy :: _ "internal")



class FetchStatusError (status :: Status) startErrors endErrors | status -> startErrors endErrors where
    createError :: Proxy status -> (String -> Builder (Record startErrors) (Record endErrors))

instance (Lacks "ok" startErrors) =>
    FetchStatusError Ok startErrors (ok :: String | startErrors) where
    createError _ = Builder.insert (Proxy :: _ "ok")

instance (Lacks "noContent" startErrors) =>
    FetchStatusError NoContent startErrors (noContent :: String | startErrors) where
    createError _ = Builder.insert (Proxy :: _ "noContent")

instance (Lacks "badRequest" startErrors) =>
    FetchStatusError BadRequest startErrors (badRequest :: String | startErrors) where
    createError _ = Builder.insert (Proxy :: _ "badRequest")

instance (Lacks "notAuthorized" startErrors) =>
    FetchStatusError NotAuthorized startErrors (notAuthorized :: String | startErrors) where
    createError _ = Builder.insert (Proxy :: _ "notAuthorized")

instance (Lacks "forbidden" startErrors) =>
    FetchStatusError Forbidden startErrors (forbidden :: String | startErrors) where
    createError _ = Builder.insert (Proxy :: _ "forbidden")

instance (Lacks "internal" startErrors) =>
    FetchStatusError Internal startErrors (internal :: String | startErrors) where
    createError _ = Builder.insert (Proxy :: _ "internal")



fetchStatus'
    :: forall status startErrors endErrors
    .  FetchStatusError status startErrors endErrors
    => Proxy status
    -> Int
    -> FetchRes.Response
    -> Async (Builder (Record startErrors) (Record endErrors)) Unit
fetchStatus' proxy expected response =
    if status response == expected
    then pure unit
    else left $ createError proxy $ "Wrong status. Expected " <> show expected <> ", got " <> (show $ status response)

class FetchStatus (status :: Status) startErrors endErrors | status -> startErrors endErrors where
    fetchStatus
        :: Proxy status
        -> FetchRes.Response
        -> Async
            (Builder (Record startErrors) (Record endErrors))
            Unit

instance (Lacks "ok" startErrors) =>
    FetchStatus Ok startErrors (ok :: String | startErrors) where
    fetchStatus proxy response = fetchStatus' proxy 200 response

instance (Lacks "noContent" startErrors) =>
    FetchStatus NoContent startErrors (noContent :: String | startErrors) where
    fetchStatus proxy response = fetchStatus' proxy 204 response

instance (Lacks "badRequest" startErrors) =>
    FetchStatus BadRequest startErrors (badRequest :: String | startErrors) where
    fetchStatus proxy response = fetchStatus' proxy 400 response

instance (Lacks "internal" startErrors) =>
    FetchStatus Internal startErrors (internal :: String | startErrors) where
    fetchStatus proxy response = fetchStatus' proxy 500 response



class FetchResponse (response :: Response) startErrors endErrors (realBody :: Type) results | response -> startErrors endErrors realBody results where
    fetchResponse'
        :: Proxy response
        -> FetchRes.Response
        -> Async
            (Builder (Record startErrors) (Record endErrors))
            (Variant results)

instance
    ( FetchStatusError status startErrors endErrors
    , FetchStatusResult status realBody results
    , FetchStatus status startErrors endErrors
    , FetchBody body realBody
    ) =>
    FetchResponse (FullResponse status body) startErrors endErrors realBody results where
    fetchResponse' _ response = do
        let statusProxy = (Proxy :: _ status)
        fetchStatus statusProxy response
        body <- fetchResponseBody (Proxy :: _ body) response # lmap (createError statusProxy)
        pure $ createResult statusProxy body

instance
    ( FetchResponse leftResponse startErrors mid realBody results
    , FetchResponse rightResponse mid endErrors realBody results
    ) =>
    FetchResponse (ResponseChain leftResponse rightResponse) startErrors endErrors realBody results where
    fetchResponse' _ response = let
        leftProxy = (Proxy :: _ leftResponse)
        rightProxy = (Proxy :: _ rightResponse)
        in
        fetchResponse' leftProxy response # attempt >>= (case _ of
            Left leftError -> fetchResponse' rightProxy response # attempt <#> case _ of
                Left rightError -> Left $ leftError >>> rightError
                Right rightResult -> Right rightResult
            Right leftResult -> pure $ Right leftResult)
        # giveUp

fetchResponse
    :: forall realBody response results errors errorsList
    .  FetchResponse response () errors realBody results
    => Nub errors errors
    => RowToList errors errorsList
    => ShowRecordFields errorsList errors
    => Proxy response
    -> FetchRes.Response
    -> Async String (Variant results)
fetchResponse proxy response = fetchResponse' proxy response # lmap (buildFromScratch >>> show)



type FetchOptions = { origin :: Maybe String, pathPrefix :: Maybe String, credentials :: Maybe Credentials }

defaultOptions :: FetchOptions
defaultOptions = { origin: Nothing, pathPrefix: Nothing, credentials: Nothing }



class Fetch (route :: Route) pathParams queryParams body responses | route -> pathParams queryParams body responses where
    fetch
        :: Proxy route
        -> Record pathParams
        -> Record queryParams
        -> body
        -> FetchOptions
        -> Async String (Variant responses)

instance
    ( FetchMethod method
    , FetchPath path pathParams
    , FetchQuery query queryParams
    , FetchBody requestBody realBody
    , FetchResponse response () errors realBody responses
    , Nub errors errors
    , RowToList errors errorsList
    , ShowRecordFields errorsList errors
    ) =>
    Fetch (FullRoute (FullRequest method path query requestBody) response) pathParams queryParams realBody responses where
    fetch _ pathParams queryParams realBody { origin, pathPrefix, credentials } = let
        method = fetchMethod (Proxy :: _ method)
        url = fetchUrl origin pathPrefix (Proxy :: _ path) (Proxy :: _ query) pathParams queryParams
        body = fetchRequestBody (Proxy :: _ requestBody) realBody
        options = method <> body <> maybe mempty (Fetch.credentials := _) credentials
        in
        Fetch.fetch url options # lmap message >>= fetchResponse (Proxy :: _ response)
