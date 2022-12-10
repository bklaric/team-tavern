module Veles where

-- import Prelude

-- import Async (Async, runAsync)
-- import Async as Async
-- import Browser.Async.Fetch as Fetch
-- import Browser.Async.Fetch.Response (status, text)
-- import Browser.Async.Fetch.Response as FetchResponse
-- import Browser.Fetch (body)
-- import Data.Bifunctor (bimap, lmap)
-- import Data.Either (Either(..), either, hush)
-- import Data.HTTP.Method (Method(..))
-- import Data.List ((:))
-- import Data.List as List
-- import Data.Maybe (Maybe(..))
-- import Data.Options (assoc)
-- import Data.String.NonEmpty (NonEmptyString)
-- import Data.String.NonEmpty as NonEmptyString
-- import Data.Symbol (class IsSymbol, reflectSymbol)
-- import Data.Traversable (traverse)
-- import Data.Variant (class VariantMatchCases, Variant, contract, inj, match, over)
-- import Effect (Effect)
-- import Effect.Console (log, logShow)
-- import Error (Error)
-- import Error.Class (message)
-- import Jarilo.Junction (type (<|>), type (:=))
-- import Jarilo.Types (Get, Post)
-- import Jarilo.Types (type (/), Capture, Literal, Path)
-- import Jarilo.Types (type (&), Mandatory, NoQuery, Optional)
-- import Jarilo.Types (type (!), BadRequest, Internal_, Ok, Response)
-- import Jarilo.Types (FullRoute, Route, RouteErrors)
-- import Prim.Row (class Lacks, class Nub, class Union)
-- import Prim.RowList (class RowToList)
-- import Record.Builder (Builder, build, insert)
-- import TeamTavern.Routes.Game.ViewAllGames as ViewAllGames
-- import Type.Proxy (Proxy(..))
-- import URI.Extra.QueryPairs (QueryPairs(..))
-- import URI.Path.Segment (unsafeSegmentFromString)
-- import Unsafe.Coerce (unsafeCoerce)
-- import Yoga.JSON (class ReadForeign, class ReadForeignVariant, class WriteForeign, readJSON, readJSON_, unsafeStringify, writeJSON)
-- import Yoga.JSON.Async as JsonAsync

-- type RegisterPlayer =
--     (Post { content :: String })
--     (Literal "players")
--     NoQuery
--     (  Ok { response :: Int }
--     ! BadRequestJson { error :: String }
--     ! Internal_)

-- type ViewPlayer = Get (Literal "players" / Capture "nickname" NonEmptyString) NoQuery (Ok { huehue :: Int } ! BadRequestJson String)

-- type ViewPlayers = Get (Literal "players") (Optional "game" NonEmptyString & Mandatory "teamId" Int) (Internal_)

-- type PlayerRoutes
--     =    "registerPlayer" := RegisterPlayer
--     <|> "viewPlayer"     := ViewPlayer
--     <|> "viewPlayers"    := ViewPlayers

-- junction :: ∀ t166.
--     Either
--         { viewPlayers :: Variant RouteErrors
--         , viewPlayer :: Variant RouteErrors
--         , registerPlayer :: Variant RouteErrors
--         }
--         (Variant
--             ( registerPlayer :: {}
--             , viewPlayer :: { nickname :: NonEmptyString }
--             , viewPlayers :: { teamId :: Int, game :: Maybe NonEmptyString }
--             | t166
--             )
--         )
-- junction =
--     router (Proxy :: _ PlayerRoutes) (Right POST) (unsafeSegmentFromString "players" : List.Nil) (QueryPairs [])

-- wut :: String
-- wut = case hush junction of
--     Nothing -> "nothing"
--     Just routeValues -> match
--         { registerPlayer: \{} -> "register player"
--         , viewPlayer: \{ nickname } -> "view player " <> show nickname
--         , viewPlayers: \{ teamId, game } -> "viewPlayers " <> show teamId <> " " <> show game
--         }
--         routeValues

-- class WriteRequest (route :: Route) request | route -> request where
--     writeRequest :: Proxy route -> request -> String

-- instance WriteForeign request => WriteRequest (FullRoute (Post request) path query response) request where
--     writeRequest _ = writeJSON

-- class ReadRequest (route :: Route) request | route -> request where
--     readRequest :: Proxy route -> String -> Maybe request

-- instance ReadForeign request => ReadRequest (FullRoute (Post request) path query response) request where
--     readRequest _ = readJSON_


-- class PrepareWriteResponse (route :: Route) input output | route -> input output where
--     prepareWriteResponse :: Proxy route -> Builder (Record input) (Record output)

-- instance (WriteForeign body, Lacks "ok" input) =>
--     PrepareWriteResponse (FullRoute method path query (Ok body)) input (ok :: body -> { code :: Int, body :: Maybe String } | input) where
--     prepareWriteResponse _ = insert (Proxy :: _ "ok") \body -> { code: 200, body: Just $ writeJSON body }

-- instance (WriteForeign body, Lacks "badRequest" input) =>
--     PrepareWriteResponse (FullRoute method path query (BadRequest body)) input (badRequest :: body -> { code :: Int, body :: Maybe String } | input) where
--     prepareWriteResponse _ = insert (Proxy :: _ "badRequest") \body -> { code: 400, body: Just $ writeJSON body }

-- instance (Lacks "internal" input) =>
--     PrepareWriteResponse (FullRoute method path query Internal_) input (internal :: body -> { code :: Int, body :: Maybe String } | input) where
--     prepareWriteResponse _ = insert (Proxy :: _ "internal") $ const { code: 500, body: Nothing }

-- instance
--     ( PrepareWriteResponse (FullRoute method path query leftResponse) input midput
--     , PrepareWriteResponse (FullRoute method path query rightResponse) midput output
--     ) =>
--     PrepareWriteResponse (FullRoute method path query (leftResponse ! rightResponse)) input output where
--     prepareWriteResponse _ = let
--         leftBuilder = prepareWriteResponse (Proxy :: _ (FullRoute method path query leftResponse))
--         rightBuilder = prepareWriteResponse (Proxy :: _ (FullRoute method path query rightResponse))
--         in
--         leftBuilder >>> rightBuilder

-- writeResponse :: ∀ route result outputList output wtf response.
--     PrepareWriteResponse route () output => RowToList output outputList => VariantMatchCases outputList wtf result => Union wtf () response =>
--     Proxy route -> Variant response -> result
-- writeResponse proxy response = let
--     responseHandlers = build (prepareWriteResponse proxy) {}
--     in
--     response # match responseHandlers


-- -- class PrepareReadResponse (route :: Route) input output | route -> input output where
-- --     prepareReadResponse :: Proxy route -> Builder (Record input) (Record output)

-- -- instance (ReadForeign body, Lacks "ok" input) =>
-- --     PrepareReadResponse (FullRoute method path query (Ok body)) input (ok :: Maybe String -> Variant (ok :: Either String (Maybe body) | other) | input) where
-- --     prepareReadResponse _ = insert (Proxy :: _ "ok") \body -> inj (Proxy :: _ "ok") (body # traverse readJSON # lmap show)

-- -- instance (ReadForeign body, Lacks "badRequest" input) =>
-- --     PrepareReadResponse (FullRoute method path query (BadRequest body)) input (badRequest :: Maybe String -> Variant (badRequest :: Either String (Maybe body) | other) | input) where
-- --     prepareReadResponse _ = insert (Proxy :: _ "badRequest") \body -> inj (Proxy :: _ "badRequest") (body # traverse readJSON # lmap show)

-- -- instance (Lacks "internal" input) =>
-- --     PrepareReadResponse (FullRoute method path query Internal_) input (internal :: Maybe String -> Variant (internal :: Unit | other) | input) where
-- --     prepareReadResponse _ = insert (Proxy :: _ "internal") $ const $ inj (Proxy :: _ "internal") unit

-- -- instance
-- --     ( PrepareReadResponse (FullRoute method path query leftResponse) input midput
-- --     , PrepareReadResponse (FullRoute method path query rightResponse) midput output
-- --     ) =>
-- --     PrepareReadResponse (FullRoute method path query (leftResponse ! rightResponse)) input output where
-- --     prepareReadResponse _ = let
-- --         leftBuilder = prepareReadResponse (Proxy :: _ (FullRoute method path query leftResponse))
-- --         rightBuilder = prepareReadResponse (Proxy :: _ (FullRoute method path query rightResponse))
-- --         in
-- --         leftBuilder >>> rightBuilder

-- -- readResponse proxy response = let
-- --     responseHandlers = build (prepareReadResponse proxy) {}
-- --     in
-- --     response # match responseHandlers



-- class ReadResponse (response :: Response) start end result | response -> start end result where
--     readResponse
--         :: Proxy response
--         -> FetchResponse.Response
--         -> Async (Builder (Record start) (Record end)) (Variant result)

-- instance (ReadForeign body, Lacks "ok" start) => ReadResponse (Ok body) start (ok :: String | start) (ok :: body | result) where
--     readResponse _ response =
--         case status response of
--         200 -> do
--             jsonBody <- response # text >>= JsonAsync.readJSON # lmap (\error -> insert (Proxy :: _ "ok") (show error))
--             pure $ inj (Proxy :: Proxy "ok") jsonBody
--         _ -> Async.left $ insert (Proxy :: _ "ok") "Wrong status"

-- -- instance (ReadForeign body) => ReadResponse (BadRequest body) (badRequest :: Maybe String | input) (badRequest :: Either String (Maybe body) | input) where
-- --     readResponse _ = over { badRequest: \body -> body # traverse readJSON # lmap show }

-- -- instance ReadResponse Internal_ (internal :: Maybe String | input) (internal :: Unit | input) where
-- --     readResponse _ = over { internal: const unit }

-- -- instance
-- --     ( ReadResponse leftResponse input midput
-- --     , ReadResponse rightResponse midput output
-- --     ) =>
-- --     ReadResponse (leftResponse ! rightResponse) input output where
-- --     readResponse _ = let
-- --         leftBuilder = readResponse (Proxy :: _ leftResponse)
-- --         rightBuilder = readResponse (Proxy :: _ rightResponse)
-- --         in
-- --         leftBuilder >>> rightBuilder

-- readResponse' proxy response = readResponse proxy response # lmap (flip build {} >>> unsafeStringify)



-- class ToComponent value where
--     toComponent :: value -> String

-- instance ToComponent String where
--     toComponent = identity

-- instance ToComponent Int where
--     toComponent = show

-- instance ToComponent Boolean where
--     toComponent = show

-- instance ToComponent NonEmptyString where
--     toComponent = NonEmptyString.toString



-- class Url (path :: Path) parameters | path -> parameters where
--     url :: Proxy path -> parameters -> String

-- instance IsSymbol name => Url (Literal name) parameters where
--     url _ _ = "/" <> reflectSymbol (Proxy :: _ name)

-- instance ToComponent value => Url (Capture name value) { name :: value | parameters } where
--     url _ parameters = "/" <> (toComponent $ parameters.name)

-- instance (Url leftPath parameters, Url rightPath parameters) => Url (leftPath / rightPath) parameters where
--     url _ parameters = url (Proxy :: _ leftPath) parameters <> url (Proxy :: _ rightPath) parameters

-- url' :: ∀ path424. Url path424 (Record ()) => Proxy path424 -> String
-- url' proxy = url proxy {}

-- -- class Url_ (path :: Path) where
-- --     url_ :: Proxy path -> String

-- -- instance IsSymbol name => Url_ (Literal name) where
-- --     url_ _ = "/" <> reflectSymbol (Proxy :: _ name)

-- -- instance (Url_ leftPath, Url_ rightPath) => Url_ (leftPath / rightPath) where
-- --     url_ _ = url_ (Proxy :: _ leftPath) <> url_ (Proxy :: _ rightPath)


-- -- fetch has to take:
-- -- * proxy
-- -- fetch may take:
-- -- * origin (scheme, host) if you're hitting a different origin from the current page
-- -- * path prefix (e.g. /api) if you're dealing with proxies
-- -- * path parameters, captured segments, if any
-- -- * query parameters, if any
-- -- * request body, if method is POST or PUT

-- -- origin and pathPrefix first for partial application
-- -- fetch proxy
-- -- Option.Record
-- -- ( pathParameters :: { pathParams } -- from proxy
-- -- , queryParameters :: { queryParams } -- from proxy
-- -- , body :: { body } -- from proxy
-- -- )
-- -- ( host :: String -- e.g. https://www.teamtavern.net
-- -- , pathPrefix :: Proxy pathPrefix
-- -- , credentials :: Credentials
-- -- )


-- class Fetch (route :: Route) requestBody responseBody | route -> requestBody responseBody where
--     fetch :: Proxy route -> requestBody -> responseBody

-- instance (Url path {}, ReadResponse response () errors responseBody, WriteForeign requestBody) =>
--     Fetch (FullRoute (Post requestBody) path query response) requestBody (Async String (Variant responseBody)) where
--     fetch _ request = Fetch.fetch (url' (Proxy :: _ path)) (body `assoc` (writeJSON request))
--         # lmap message
--         >>= readResponse' (Proxy :: _ response)

-- instance (Url path {}, ReadResponse response () errors responseBody, WriteForeign requestBody) =>
--     Fetch (FullRoute Get path query response) requestBody (Async String (Variant responseBody)) where
--     fetch _ _ = Fetch.fetch_ (url' (Proxy :: _ path))
--         # lmap message
--         >>= readResponse' (Proxy :: _ response)



-- type FetchTest = Get (Literal "api" / Literal "games") NoQuery (Ok ViewAllGames.OkContent)

-- -- fetchTest :: Async String Unit
-- fetchTest :: Effect Unit
-- fetchTest = fetch (Proxy :: _ FetchTest) { anything: 123 } <#> (match
--     { ok: \games -> show games
--     }) # runAsync (either log log)

-- -- test = readResponse (Proxy :: _ RegisterPlayer) (inj (Proxy :: _ "ok") (Just """{ "response": "123" }"""))

-- -- test2 = test # match
-- --     { badRequest: either identity show
-- --     , internal: const "internal"
-- --     , ok: either identity show
-- --     }

-- main :: Effect Unit
-- main = do
--     log $ show $ readRequest (Proxy :: _ RegisterPlayer) $ writeRequest (Proxy :: _ RegisterPlayer) { content: "aoeu" }
--     log $ unsafeCoerce $ build (prepareWriteResponse (Proxy :: _ RegisterPlayer)) {}
--     log $ show $ writeResponse (Proxy :: _ RegisterPlayer) (inj (Proxy :: _ "badRequest") {error: "oh no"})
--     -- log $ unsafeCoerce $ build (prepareReadResponse (Proxy :: _ RegisterPlayer)) {}
--     -- log $ show $ writeJSON $ readResponse (Proxy :: _ RegisterPlayer) (inj (Proxy :: _ "badRequest") (Just """{ "error": "haha" }""")) -- $ writeResponse (Proxy :: _ RegisterPlayer) (inj (Proxy :: _ "badRequest") {error: "oh no"})
--     -- log test2

-- type Bruh other = ( a :: String, b :: Int | other)

-- funny :: ∀ before after other bruhLike. Union bruhLike after (Bruh after) => Record (a :: String | after) -> Effect Unit
-- funny bruhLike = do
--     log $ unsafeCoerce bruhLike.a

-- huehue = funny { a: "lmao", yooo: "wtf" }
