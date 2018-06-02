module Main where

import Prelude

import Data.Either (Either(Left, Right))
import Data.HTTP.Method (CustomMethod, Method)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Options (Options, (:=))
import Data.String.NonEmpty (toString)
import Data.Variant (match)
import Effect (Effect)
import Node.Server (ListenOptions(..))
import Perun.Request (Request)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Perun.Server (run_)
import Perun.Url (Url, pathSegments, queryPairs)
import Postgres.Client.Config (ClientConfig, database, host, password, port, user)
import Postgres.Pool (Pool)
import Postgres.Pool as Pool
import Postmark.Client (Client)
import Postmark.Client as Postmark
import Routing.Junction (JunctionProxy(JunctionProxy), junctionRouter')
import TeamTavern.Architecture.Environment (Environment(..))
import TeamTavern.Player.Register.Run (handleRegister)
import TeamTavern.Player.Routes (TeamTavernRoutes)

listenOptions :: ListenOptions
listenOptions = TcpListenOptions
    { port: Just 8080
    , host: Nothing
    , backlog: Nothing
    , exclusive: Nothing
    }

teamTavernRoutes = JunctionProxy :: JunctionProxy TeamTavernRoutes

clientConfig :: Options ClientConfig
clientConfig =
    user := "bklaric"
    <> password := "bklaric"
    <> host := "localhost"
    <> port := 5432
    <> database := "team_tavern"

handleRequest :: Pool -> Client -> Either CustomMethod Method -> Url -> Body -> (Response -> Effect Unit) -> Effect Unit
handleRequest pool client method url body respond =
    case junctionRouter' teamTavernRoutes method (pathSegments url) (queryPairs url) of
    Left _ -> respond { statusCode: 404, content: "404 Not Found" }
    Right routeValues -> routeValues # match
        { viewPlayers: const $ respond { statusCode: 200, content: "You're viewing all players." }
        , viewPlayer: \{nickname} -> respond { statusCode: 200, content: "You're viewing player " <> toString nickname <> "." }
        , registerPlayer: const $ handleRegister Local pool client body respond
        }

handleInvalidUrl :: Pool -> Client -> Request -> (Response -> Effect Unit) -> Effect Unit
handleInvalidUrl pool client { method, url, body } respond =
    case url of
    Right url' -> handleRequest pool client method url' body respond
    Left url' -> respond { statusCode: 400, content: "Couldn't parse url '" <> url' <> "'." }

main :: Effect Unit
main = do
    pool <- Pool.create mempty clientConfig
    client <- Postmark.create "d763b189-d006-4e4a-9d89-02212ccd87f5"
    run_ listenOptions (handleInvalidUrl pool client)
