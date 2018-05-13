module Main where

import Prelude

import Body (Body, fromRequest, readAsUtf8)
import Control.Monad.Except (runExcept)
import Data.Bifunctor (lmap)
import Data.Either (Either(Right, Left))
import Data.Foreign.Generic (genericDecodeJSON)
import Data.Foreign.Generic.Types (SumEncoding(..))
import Data.Foreign.Generic.Types as Generic
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (CustomMethod, Method, fromString)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Options (Options, (:=))
import Data.String.NonEmpty (toString)
import Data.Variant (match)
import Effect (Effect)
import Error.Class (message)
import Node.Http.Server (HttpServer, create_)
import Node.Http.Server.Request (Request, method, url)
import Node.Http.Server.Response (Response, setStatusCode)
import Node.Server (ListenOptions(..), listen_)
import Node.Stream.Writable (endString__)
import Postgres.Client.Config (ClientConfig, database, host, password, port, user)
import Postgres.Pool as Pool
import Postgres.Query (Query(Query), QueryParameter(QueryParameter), query)
import Routes (TeamTavernRoutes)
import Routing.Junction (JunctionProxy(JunctionProxy), junctionRouter')
import Url (Url, parseUrl, pathSegments, queryPairs)

listenOptions :: ListenOptions
listenOptions = TcpListenOptions
    { port: Just 8080
    , host: Nothing
    , backlog: Nothing
    , exclusive: Nothing
    }

type HttpRequest =
    { method :: Either CustomMethod Method
    , url :: Either String Url
    , body :: Body
    }

readRequest :: Request -> HttpRequest
readRequest request =
    { method: request # method # fromString # case _ of
        Left m -> Right m
        Right cm -> Left cm
    , url: request # url # parseUrl # lmap (const $ url request)
    , body: fromRequest request
    }

type HttpResponse =
    { statusCode :: Int
    , content :: String
    }

type RequestHandler =
    HttpRequest -> (HttpResponse -> Effect Unit) -> Effect Unit

respond :: Response -> HttpResponse -> Effect Unit
respond response { statusCode, content } = do
    setStatusCode statusCode response
    endString__ content response # void

createFancy :: RequestHandler -> Effect HttpServer
createFancy handler = create_ \request response ->
    handler (readRequest request) (respond response)

runServer :: ListenOptions -> RequestHandler -> Effect Unit
runServer listenOptions handler = do
    server <- createFancy handler
    server # listen_ listenOptions

teamTavernRoutes = (JunctionProxy :: JunctionProxy TeamTavernRoutes)

newtype RegisterPlayerModel = RegisterPlayerModel
    { email :: String
    , nickname :: String
    }

derive instance genericRegisterPlayerModel :: Generic RegisterPlayerModel _

genericOptions :: Generic.Options
genericOptions =
    { sumEncoding: TaggedObject
        { tagFieldName: "tag"
        , contentsFieldName: "data"
        , constructorTagTransform: id
        }
    , unwrapSingleConstructors: true
    , unwrapSingleArguments: true
    , fieldTransform: id
    }

clientConfig :: Options ClientConfig
clientConfig =
    user := "bklaric"
    <> password := "bklaric"
    <> host := "localhost"
    <> port := 5432
    <> database := "team_tavern"

registerPlayer :: Body -> (HttpResponse -> Effect Unit) -> Effect Unit
registerPlayer body respond = void $
    body # readAsUtf8 \bodyString ->
        case runExcept $ genericDecodeJSON genericOptions bodyString of
        Left errors -> respond { statusCode: 400, content: "Couldnt parse body '" <> bodyString <> "' because: " <> show errors }
        Right (RegisterPlayerModel { email, nickname }) -> do
            Pool.create mempty clientConfig >>= query
                (Query "insert into player (email, nickname, token) values ($1, $2, $3)")
                (QueryParameter <$> [email, nickname, "aoeu654aoeu654"])
                case _ of
                Left error -> respond { statusCode: 400, content: "Error inserting in the database: " <> message error }
                Right result -> respond { statusCode: 200, content: "Looks good: " <> email <> ", " <> nickname }

requestHandler3 :: Either CustomMethod Method -> Url -> Body -> (HttpResponse -> Effect Unit) -> Effect Unit
requestHandler3 method url body respond =
    case junctionRouter' teamTavernRoutes method (pathSegments url) (queryPairs url) of
    Left _ -> respond { statusCode: 404, content: "404 Not Found" }
    Right routeValues -> routeValues # match
        { viewPlayers: const $ respond { statusCode: 200, content: "You're viewing all players." }
        , viewPlayer: \{nickname} -> respond { statusCode: 200, content: "You're viewing player " <> toString nickname <> "." }
        , registerPlayer: const $ registerPlayer body respond
        }

requestHandler2 :: HttpRequest -> (HttpResponse -> Effect Unit) -> Effect Unit
requestHandler2 { method, url, body } respond =
    case url of
    Right url' -> requestHandler3 method url' body respond
    Left url' -> respond { statusCode: 400, content: "Couldn't parse url '" <> url' <> "'." }

main :: Effect Unit
main = runServer listenOptions requestHandler2
