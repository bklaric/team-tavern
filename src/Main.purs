module Main where

import Prelude

import Body (Body, fromRequest, readAsUtf8)
import Data.Either (Either, either)
import Data.HTTP.Method (CustomMethod, Method(..), fromString)
import Data.List (List(Nil))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Global.Unsafe (unsafeStringify)
import Node.Http.Server (Request, Response, create)
import Node.Http.ServerRequest (method, url)
import Node.Server (ListenOptions(..), listen_)
import Node.Stream.Writable (end__, writeString__)
import Routing.Junction (JunctionProxy(JunctionProxy), junctionRouter')
import Text.Parsing.Parser (ParseError)
import URI.Extra.QueryPairs (QueryPairs(QueryPairs))
import Url (Url, parseUrl, pathSegments, queryPairs)
import Routes (TeamTavernRoutes)

listenOptions :: ListenOptions
listenOptions = TcpListenOptions
    { port: Just 8080
    , host: Nothing
    , backlog: Nothing
    , exclusive: Nothing
    }

requestHandler :: Request -> Response -> Effect Unit
requestHandler request response = do
    fromRequest request # readAsUtf8 (\body -> do
        writeString__ body response # void
        writeString__ "\n" response # void
        let requestMethod = method request # fromString
        let requestUrl = url request # parseUrl
        let routedUrl = junctionRouter'
                            (JunctionProxy :: JunctionProxy TeamTavernRoutes)
                            (either id (const GET) requestMethod)
                            (either (const Nil) pathSegments requestUrl)
                            (either (const $ QueryPairs []) queryPairs requestUrl)
        writeString__ (either show show requestMethod) response # void
        writeString__ "\n" response # void
        writeString__ (either show show requestUrl) response # void
        writeString__ "\n" response # void
        writeString__ (unsafeStringify routedUrl) response # void
        end__ response # void) # void

type HttpRequest =
    { method :: Either Method CustomMethod
    , url :: Either ParseError Url
    , body :: Body
    }

readRequest :: Request -> HttpRequest
readRequest request =
    { method: request # method # fromString
    , url: request # url # parseUrl
    , body: fromRequest request
    }

main :: Effect Unit
main = do
    server <- create requestHandler
    server # listen_ listenOptions
