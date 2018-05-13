module Main where

import Prelude

import Body (Body, fromRequest)
import Data.Bifunctor (lmap)
import Data.Either (Either(Right, Left))
import Data.HTTP.Method (CustomMethod, Method, fromString)
import Data.Maybe (Maybe(..))
import Data.String.NonEmpty (toString)
import Data.Variant (match)
import Effect (Effect)
import Node.Http.Server (HttpServer, Request, create)
import Node.Http.ServerRequest (method, url)
import Node.Http.ServerResponse (setStatusCode)
import Node.Server (ListenOptions(..), listen_)
import Node.Stream.Writable (endString__)
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

createFancy :: (HttpRequest -> Effect HttpResponse) -> Effect HttpServer
createFancy handler = create \request response -> do
    { statusCode, content } <- request # readRequest # handler
    setStatusCode statusCode response
    endString__ content response # void

runServer
    :: ListenOptions
    -> (HttpRequest -> Effect HttpResponse)
    -> Effect Unit
runServer listenOptions handler = do
    server <- createFancy handler
    server # listen_ listenOptions

teamTavernRoutes = (JunctionProxy :: JunctionProxy TeamTavernRoutes)

requestHandler3 method url body =
    case junctionRouter' teamTavernRoutes method (pathSegments url) (queryPairs url) of
    Left _ -> pure { statusCode: 404, content: "404 Not Found" }
    Right routeValues -> routeValues # match
        { viewPlayers: const $ pure { statusCode: 200, content: "You're viewing all players." }
        , viewPlayer: \{nickname} -> pure { statusCode: 200, content: "You're viewing player " <> toString nickname <> "." }
        , registerPlayer: const $ pure { statusCode: 200, content: "You're registering a player." }
        }

requestHandler2 { method, url, body } =
    case method, url of
    Right method', Right url' -> requestHandler3 method' url' body
    _, _ -> pure { statusCode: 400, content: "You fugen suck, lmao!" }

main :: Effect Unit
main = runServer listenOptions requestHandler2
