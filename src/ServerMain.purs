module ServerMain where

import Prelude

import Async (Async, fromEffect)
import Control.Bind (bindFlipped)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Maybe.Trans (lift)
import Data.Either (Either(..), either, note)
import Data.HTTP.Method (CustomMethod, Method(..))
import Data.Int (fromString)
import Data.List.NonEmpty as NEL
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Options (Options, (:=))
import Data.Tuple (Tuple(..))
import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import Global.Unsafe (unsafeStringify)
import Jarilo.Junction (JunctionProxy(..), router)
import MultiMap as MultiMap
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
import TeamTavern.Architecture.Deployment (Deployment(..))
import TeamTavern.Architecture.Deployment as Deployment
import TeamTavern.Game.Create (handleCreate)
import TeamTavern.Game.View as Game
import TeamTavern.Player.Register.Run (handleRegister)
import TeamTavern.Player.Session.Prepare.Run (handlePrepare)
import TeamTavern.Player.Session.Start.Run (handleStart)
import TeamTavern.Player.Update (handleUpdate)
import TeamTavern.Player.View as Player
import TeamTavern.Routes (TeamTavernRoutes)

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

handleRequest
    :: Pool
    -> Maybe Client
    -> Either CustomMethod Method
    -> Url
    -> Map String String
    -> Body
    -> (forall left. Async left Response)
handleRequest pool client method url cookies body =
    case router teamTavernRoutes method (pathSegments url) (queryPairs url) of
    Left errors ->
        if method == Right OPTIONS
        then pure
            { statusCode: 200
            , headers: MultiMap.fromFoldable
                [ Tuple "Access-Control-Allow-Origin" $ NEL.singleton "http://localhost:1337"
                , Tuple "Access-Control-Allow-Methods" $ NEL.singleton "GET, POST, DELETE, PUT, PATCH, HEAD, OPTIONS"
                , Tuple "Access-Control-Max-Age" $ NEL.singleton $ show $ (top :: Int)
                , Tuple "Access-Control-Allow-Credentials" $ NEL.singleton "true"
                ]
            , content: mempty
            }
        else do
            fromEffect $ log $
                "404 for method " <> show method <> " and url " <> show url
            fromEffect $ log $ unsafeStringify errors
            pure { statusCode: 404, headers: MultiMap.empty, content: unsafeStringify errors }
    Right routeValues -> routeValues # match
        { viewPlayers: const $ pure
            { statusCode: 200
            , headers: MultiMap.empty
            , content: "You're viewing all players."
            }
        , viewPlayer: \{ nickname } ->
            Player.handleView pool nickname
        , registerPlayer:
            const $ handleRegister pool client cookies body
        , prepareSession: \{ nickname } ->
            handlePrepare pool client nickname body cookies
        , startSession: \{ nickname } ->
            handleStart pool nickname cookies body
        , updatePlayer: \{ nickname } ->
            handleUpdate pool nickname cookies body
        , createGame:
            const $ handleCreate pool cookies body
        , viewGame: \{ handle } ->
            Game.handleView pool handle
        }
        <#> (\response -> response { headers = response.headers <> MultiMap.fromFoldable
                [ Tuple "Access-Control-Allow-Origin" $ NEL.singleton "http://localhost:1337"
                , Tuple "Access-Control-Allow-Methods" $ NEL.singleton "GET, POST, DELETE, PUT, PATCH, HEAD, OPTIONS"
                , Tuple "Access-Control-Max-Age" $ NEL.singleton $ show $ (top :: Int)
                , Tuple "Access-Control-Allow-Credentials" $ NEL.singleton "true"
                ]})

handleInvalidUrl
    :: Pool
    -> Maybe Client
    -> Request
    -> (forall left. Async left Response)
handleInvalidUrl pool client { method, url, cookies, body } =
    case url of
    Right url' -> handleRequest pool client method url' cookies body
    Left url' -> pure
        { statusCode: 400
        , headers: MultiMap.empty
        , content: "Couldn't parse url '" <> url' <> "'."
        }

main :: Effect Unit
main = either log pure =<< runExceptT do
    deployment <- loadDeployment
    pool <- createPostgresPool
    client <- createPostmarkClient deployment
    lift $ run_ listenOptions (handleInvalidUrl pool client)
