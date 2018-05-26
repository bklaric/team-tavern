module Main where

import Prelude

import Data.Either (Either(Left, Right))
import Data.Foreign.Generic.Types (SumEncoding(..))
import Data.Foreign.Generic.Types as Generic
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
import Routing.Junction (JunctionProxy(JunctionProxy), junctionRouter')
import TeamTavern.Player.Register (registerPlayerHandler)
import TeamTavern.Player.Routes (TeamTavernRoutes)

listenOptions :: ListenOptions
listenOptions = TcpListenOptions
    { port: Just 8080
    , host: Nothing
    , backlog: Nothing
    , exclusive: Nothing
    }

teamTavernRoutes = (JunctionProxy :: JunctionProxy TeamTavernRoutes)

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

requestHandler :: Pool -> Either CustomMethod Method -> Url -> Body -> (Response -> Effect Unit) -> Effect Unit
requestHandler pool method url body respond =
    case junctionRouter' teamTavernRoutes method (pathSegments url) (queryPairs url) of
    Left _ -> respond { statusCode: 404, content: "404 Not Found" }
    Right routeValues -> routeValues # match
        { viewPlayers: const $ respond { statusCode: 200, content: "You're viewing all players." }
        , viewPlayer: \{nickname} -> respond { statusCode: 200, content: "You're viewing player " <> toString nickname <> "." }
        , registerPlayer: const $ registerPlayerHandler pool body respond
        }

invalidUrlHandler :: Pool -> Request -> (Response -> Effect Unit) -> Effect Unit
invalidUrlHandler pool { method, url, body } respond =
    case url of
    Right url' -> requestHandler pool method url' body respond
    Left url' -> respond { statusCode: 400, content: "Couldn't parse url '" <> url' <> "'." }

main :: Effect Unit
main = do
    pool <- Pool.create mempty clientConfig
    run_ listenOptions (invalidUrlHandler pool)
