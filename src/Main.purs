module Main where

import Prelude

import Async (Async)
import Control.Bind (bindFlipped)
import Control.Monad.Eff.Console (log)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Maybe.Trans (lift)
import Data.Either (Either(..), either, note)
import Data.HTTP.Method (CustomMethod, Method)
import Data.Int (fromString)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Options (Options, (:=))
import Data.String.NonEmpty (toString)
import Data.Variant (match)
import Effect (Effect)
import MultiMap (empty)
import Node.Process (lookupEnv)
import Node.Server (ListenOptions(..))
import Perun.Async.Server (run_)
import Perun.Request (Request)
import Perun.Request.Body (Body)
import Perun.Response (Response)
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
import TeamTavern.Player.SignIn.Run (handleSignIn)
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

handleRequest :: Pool -> Maybe Client -> Either CustomMethod Method -> Url -> Map String String -> Body -> (forall left. Async left Response)
handleRequest pool client method url cookies body =
    case junctionRouter' teamTavernRoutes method (pathSegments url) (queryPairs url) of
    Left _ -> pure { statusCode: 404, headers: empty, content: "404 Not Found" }
    Right routeValues -> routeValues # match
        { viewPlayers: const $ pure { statusCode: 200, headers: empty, content: "You're viewing all players." }
        , viewPlayer: \{nickname} -> pure { statusCode: 200, headers: empty, content: "You're viewing player " <> toString nickname <> "." }
        , registerPlayer: const $ handleRegister pool client cookies body
        , signInPlayer: \{ nickname } -> handleSignIn pool nickname cookies body
        }

handleInvalidUrl :: Pool -> Maybe Client -> Request -> (forall left. Async left Response)
handleInvalidUrl pool client { method, url, cookies, body } =
    case url of
    Right url' -> handleRequest pool client method url' cookies body
    Left url' -> pure { statusCode: 400, headers: empty, content: "Couldn't parse url '" <> url' <> "'." }

main :: Effect Unit
main = either (unsafeCoerce log) pure =<< runExceptT do
    deployment <- loadDeployment
    pool <- createPostgresPool
    client <- createPostmarkClient deployment -- "d763b189-d006-4e4a-9d89-02212ccd87f5"
    lift $ run_ listenOptions (handleInvalidUrl pool client)
