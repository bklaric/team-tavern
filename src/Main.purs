module Main where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foreign.Generic (genericDecodeJSON)
import Data.Foreign.Generic.Types (SumEncoding(..))
import Data.Foreign.Generic.Types as Generic
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (CustomMethod, Method)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Options (Options, (:=))
import Data.String.NonEmpty (toString)
import Data.Variant (match)
import Effect (Effect)
import Error.Class (message)
import Node.Server (ListenOptions(..))
import Perun.Request (Request)
import Perun.Request.Body (Body, readAsUtf8)
import Perun.Response (Response)
import Perun.Server (run_)
import Perun.Url (Url, pathSegments, queryPairs)
import Postgres.Client.Config (ClientConfig, database, host, password, port, user)
import Postgres.Pool as Pool
import Postgres.Query (Query(..), QueryParameter(..), query)
import Routes (TeamTavernRoutes)
import Routing.Junction (JunctionProxy(..), junctionRouter')

listenOptions :: ListenOptions
listenOptions = TcpListenOptions
    { port: Just 8080
    , host: Nothing
    , backlog: Nothing
    , exclusive: Nothing
    }

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

registerPlayer :: Body -> (Response -> Effect Unit) -> Effect Unit
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

requestHandler :: Either CustomMethod Method -> Url -> Body -> (Response -> Effect Unit) -> Effect Unit
requestHandler method url body respond =
    case junctionRouter' teamTavernRoutes method (pathSegments url) (queryPairs url) of
    Left _ -> respond { statusCode: 404, content: "404 Not Found" }
    Right routeValues -> routeValues # match
        { viewPlayers: const $ respond { statusCode: 200, content: "You're viewing all players." }
        , viewPlayer: \{nickname} -> respond { statusCode: 200, content: "You're viewing player " <> toString nickname <> "." }
        , registerPlayer: const $ registerPlayer body respond
        }

invalidUrlHandler :: Request -> (Response -> Effect Unit) -> Effect Unit
invalidUrlHandler { method, url, body } respond =
    case url of
    Right url' -> requestHandler method url' body respond
    Left url' -> respond { statusCode: 400, content: "Couldn't parse url '" <> url' <> "'." }

main :: Effect Unit
main = run_ listenOptions invalidUrlHandler
