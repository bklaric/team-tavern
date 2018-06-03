module Main where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Eff.Console (log)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Maybe.Trans (lift)
import Data.Either (Either(..), either, note)
import Data.HTTP.Method (CustomMethod, Method)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Options (Options, (:=))
import Data.String.NonEmpty (toString)
import Data.Variant (match)
import Effect (Effect)
import Node.Process (lookupEnv)
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
import Routing.Junction (JunctionProxy(..), junctionRouter')
import TeamTavern.Architecture.Deployment (Deployment(..))
import TeamTavern.Architecture.Deployment as Deployment
import TeamTavern.Player.Register.Run (handleRegister)
import TeamTavern.Player.Routes (TeamTavernRoutes)
import Unsafe.Coerce (unsafeCoerce)

listenOptions :: ListenOptions
listenOptions = TcpListenOptions
    { port: Just 8080
    , host: Nothing
    , backlog: Nothing
    , exclusive: Nothing
    }

loadPostgresVariables :: ExceptT String Effect
    { user :: String
    , password :: String
    , host :: String
    , port :: Int
    , database :: String
    }
loadPostgresVariables = do
    user <- lookupEnv "PGUSER"
        <#> note ("Couldn't read variable PGUSER.") # ExceptT
    password <- lookupEnv "PGPASSWORD"
        <#> note ("Couldn't read variable PGPASSWORD.") # ExceptT
    host <- lookupEnv "PGHOST"
        <#> note ("Couldn't read variable PGHOST.") # ExceptT
    port <- lookupEnv "PGPORT" <#> bindFlipped fromString
        <#> note ("Couldn't read variable PGPORT.") # ExceptT
    database <- lookupEnv "PGDATABASE"
        <#> note ("Couldn't read variable PGDATABASE.") # ExceptT
    pure { user, password, host, port, database }

createPostgresConfig ::
    { user :: String
    , password :: String
    , host :: String
    , port :: Int
    , database :: String
    }
    -> Options ClientConfig
createPostgresConfig variables =
    user := variables.user
    <> password := variables.password
    <> host := variables.host
    <> port := variables.port
    <> database := variables.database

createPostgresPool :: ExceptT String Effect Pool
createPostgresPool = do
    postgresConfig <- loadPostgresVariables <#> createPostgresConfig
    lift $ Pool.create mempty postgresConfig

loadDeployment :: ExceptT String Effect Deployment
loadDeployment =
    lookupEnv "DEPLOYMENT"
    <#> bindFlipped Deployment.fromString
    <#> note "Couldn't read variable DEPLOYMENT."
    # ExceptT

loadPostmarkApiKey :: ExceptT String Effect String
loadPostmarkApiKey =
    lookupEnv "POSTMARK"
    <#> note "Couldn't read variable POSTMARK."
    # ExceptT

createPostmarkClient :: Deployment -> ExceptT String Effect (Maybe Client)
createPostmarkClient =
    case _ of
    Local -> pure Nothing
    Cloud -> do
        apiKey <- loadPostmarkApiKey
        lift $ Just <$> Postmark.create apiKey

teamTavernRoutes = JunctionProxy :: JunctionProxy TeamTavernRoutes

handleRequest :: Pool -> Maybe Client -> Either CustomMethod Method -> Url -> Body -> (Response -> Effect Unit) -> Effect Unit
handleRequest pool client method url body respond =
    case junctionRouter' teamTavernRoutes method (pathSegments url) (queryPairs url) of
    Left _ -> respond { statusCode: 404, content: "404 Not Found" }
    Right routeValues -> routeValues # match
        { viewPlayers: const $ respond { statusCode: 200, content: "You're viewing all players." }
        , viewPlayer: \{nickname} -> respond { statusCode: 200, content: "You're viewing player " <> toString nickname <> "." }
        , registerPlayer: const $ handleRegister pool client body respond
        }

handleInvalidUrl :: Pool -> Maybe Client -> Request -> (Response -> Effect Unit) -> Effect Unit
handleInvalidUrl pool client { method, url, body } respond =
    case url of
    Right url' -> handleRequest pool client method url' body respond
    Left url' -> respond { statusCode: 400, content: "Couldn't parse url '" <> url' <> "'." }

main :: Effect Unit
main = either (unsafeCoerce log) pure =<< runExceptT do
    deployment <- loadDeployment
    pool <- createPostgresPool
    client <- createPostmarkClient deployment -- "d763b189-d006-4e4a-9d89-02212ccd87f5"
    lift $ run_ listenOptions (handleInvalidUrl pool client)
