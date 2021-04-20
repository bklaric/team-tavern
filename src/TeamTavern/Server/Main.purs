module TeamTavern.Server.Main where

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
import Data.MultiMap as MultiMap
import Data.Options (Options, (:=))
import Data.Tuple (Tuple(..))
import Data.Variant (match)
import Effect (Effect)
import Effect.Console (log)
import Jarilo.Junction (JunctionProxy(..), router)
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
import Sendgrid (setApiKey)
import TeamTavern.Server.Alert.Create as Alert
import TeamTavern.Server.Architecture.Deployment (Deployment)
import TeamTavern.Server.Architecture.Deployment as Deployment
import TeamTavern.Server.Boarding.Onboard as Onboard
import TeamTavern.Server.Boarding.Preboard as Preboard
import TeamTavern.Server.Game.View (view) as Game
import TeamTavern.Server.Game.ViewAll (viewAll) as Game
import TeamTavern.Server.Infrastructure.Log (logStamped, logt)
import TeamTavern.Server.Player.Register (register) as Player
import TeamTavern.Server.Player.UpdatePlayer (updatePlayer) as Player
import TeamTavern.Server.Player.View (view) as Player
import TeamTavern.Server.Profile.AddPlayerProfile (addPlayerProfile) as Profile
import TeamTavern.Server.Profile.AddTeamProfile (addTeamProfile) as Profile
import TeamTavern.Server.Profile.Routes (bundlePlayerFilters, bundleTeamFilters)
import TeamTavern.Server.Profile.UpdatePlayerProfile (updatePlayerProfile) as Profile
import TeamTavern.Server.Profile.UpdateTeamProfile (updateTeamProfile) as Profile
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame (viewPlayerProfilesByGame) as Profile
import TeamTavern.Server.Profile.ViewTeamProfilesByGame (viewTeamProfilesByGame) as Profile
import TeamTavern.Server.Routes (TeamTavernRoutes)
import TeamTavern.Server.Session.End (end) as Session
import TeamTavern.Server.Session.Start (start) as Session
import TeamTavern.Server.Team.Create (create) as Team
import TeamTavern.Server.Team.Update (update) as Team
import TeamTavern.Server.Team.View (view) as Team

listenOptions :: ListenOptions
listenOptions = TcpListenOptions
    { port: Just 8080
    , host: Just "localhost"
    , backlog: Nothing
    , exclusive: Nothing
    }

setSendGridApiKey :: ExceptT String Effect Unit
setSendGridApiKey = do
    key <- lookupEnv "SENDGRID_API_KEY" <#> note "Couldn't read variable SENDGRID_API_KEY" # ExceptT
    lift $ setApiKey key

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

teamTavernRoutes = JunctionProxy :: JunctionProxy TeamTavernRoutes

handleRequest
    :: Deployment
    -> Pool
    -> Either CustomMethod Method
    -> Url
    -> Map String String
    -> Body
    -> (forall left. Async left Response)
handleRequest deployment pool method url cookies body =
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
            fromEffect $ logStamped $ "Endpoint 404 Not Found"
            fromEffect $ logt $
                "Not found for method " <> show method <> " and url " <> show url
            fromEffect $ logt $ "Routing resulted in these errors: " <> show errors
            pure { statusCode: 404, headers: MultiMap.empty, content: show errors }
    Right routeValues -> routeValues # match
        { registerPlayer: const $
            Player.register deployment pool cookies body
        , viewPlayer:
            Player.view pool cookies
        , updatePlayer: \{ nickname } ->
            Player.updatePlayer pool nickname cookies body
        , viewTeam:
            Team.view pool
        , createTeam: const $
            Team.create pool body cookies
        , updateTeam:
            Team.update pool body cookies
        , startSession: const $
            Session.start deployment pool cookies body
        , endSession: const
            Session.end
        , viewAllGames: const $
            Game.viewAll pool
        , viewGame: \{ handle } ->
            Game.view pool handle cookies
        , addPlayerProfile: \identifiers ->
            Profile.addPlayerProfile pool identifiers cookies body
        , addTeamProfile:
            Profile.addTeamProfile pool cookies body
        , updatePlayerProfile: \identifiers ->
            Profile.updatePlayerProfile pool identifiers cookies body
        , updateTeamProfile:
            Profile.updateTeamProfile pool cookies body
        , viewPlayerProfilesByGame: \filters @ { handle, page, timezone } ->
            Profile.viewPlayerProfilesByGame pool handle page timezone $ bundlePlayerFilters filters
        , viewTeamProfilesByGame: \filters @ { handle, page, timezone } ->
            Profile.viewTeamProfilesByGame pool handle page timezone $ bundleTeamFilters filters
        , onboard: const $
            Onboard.onboard pool cookies body
        , preboard: const $
            Preboard.preboard deployment pool cookies body
        , createAlert: const $
            Alert.createAlert pool body
        }
        <#> (\response -> response { headers = response.headers <> MultiMap.fromFoldable
                [ Tuple "Access-Control-Allow-Origin" $ NEL.singleton "http://localhost:1337"
                , Tuple "Access-Control-Allow-Methods" $ NEL.singleton "GET, POST, DELETE, PUT, PATCH, HEAD, OPTIONS"
                , Tuple "Access-Control-Max-Age" $ NEL.singleton $ show $ (top :: Int)
                , Tuple "Access-Control-Allow-Credentials" $ NEL.singleton "true"
                ]})

handleInvalidUrl :: Deployment -> Pool -> Request -> (forall left. Async left Response)
handleInvalidUrl deployment pool { method, url, cookies, body } =
    case url of
    Right url' -> handleRequest deployment pool method url' cookies body
    Left url' -> pure
        { statusCode: 400
        , headers: MultiMap.empty
        , content: "Couldn't parse url '" <> url' <> "'."
        }

main :: Effect Unit
main = either log pure =<< runExceptT do
    deployment <- loadDeployment
    pool <- createPostgresPool
    setSendGridApiKey
    lift $ run_ listenOptions (handleInvalidUrl deployment pool)
