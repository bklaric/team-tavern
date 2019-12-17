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
import Global.Unsafe (unsafeStringify)
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
import Postmark.Client (Client)
import Postmark.Client as Postmark
import TeamTavern.Server.Architecture.Deployment (Deployment(..))
import TeamTavern.Server.Architecture.Deployment as Deployment
import TeamTavern.Server.Conversation.Start (start) as Conversation
import TeamTavern.Server.Conversation.View (view) as Conversation
import TeamTavern.Server.Conversation.ViewAll (viewAll) as Conversation
import TeamTavern.Server.Game.Create (create) as Game
import TeamTavern.Server.Game.Update (handleUpdate) as Game
import TeamTavern.Server.Game.View (handleView) as Game
import TeamTavern.Server.Game.ViewAll (handleViewAll) as Game
import TeamTavern.Server.Infrastructure.Log (logStamped, logt)
import TeamTavern.Server.Password.Forgot (forgot) as Password
import TeamTavern.Server.Password.Reset (reset) as Password
import TeamTavern.Server.Player.Register (register) as Player
import TeamTavern.Server.Player.Update (update) as Player
import TeamTavern.Server.Player.View (view) as Player
import TeamTavern.Server.Player.ViewAccount (viewAccount) as Player
import TeamTavern.Server.Player.ViewHeader (viewHeader) as Player
import TeamTavern.Server.Profile.Create (create) as Profile
import TeamTavern.Server.Profile.Update (update) as Profile
import TeamTavern.Server.Profile.ViewByGame (viewByGame) as Profile
import TeamTavern.Server.Profile.ViewByPlayer (viewByPlayer) as Profile
import TeamTavern.Server.Routes (TeamTavernRoutes)
import TeamTavern.Server.Session.End (end) as Session
import TeamTavern.Server.Session.Start (start) as Session

listenOptions :: ListenOptions
listenOptions = TcpListenOptions
    { port: Just 8080
    , host: Just "localhost"
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
            fromEffect $ logStamped $ "Endpoint 404 Not Found"
            fromEffect $ logt $
                "Not found for method " <> show method <> " and url " <> show url
            fromEffect $ logt $ "Routing resulted in these errors: " <> unsafeStringify errors
            pure { statusCode: 404, headers: MultiMap.empty, content: unsafeStringify errors }
    Right routeValues -> routeValues # match
        { registerPlayer: const $
            Player.register pool client cookies body
        , viewPlayer: \{ nickname } ->
            Player.view pool nickname
        , viewPlayerHeader: \{ id } ->
            Player.viewHeader pool id
        , viewPlayerAccount: \{ nickname } ->
            Player.viewAccount pool nickname cookies
        , updatePlayer: \{ nickname } ->
            Player.update pool nickname cookies body
        , forgotPassword: const $
            Password.forgot pool client cookies body
        , resetPassword: const $
            Password.reset pool cookies body
        , startSession: const $
            Session.start pool cookies body
        , endSession: const
            Session.end
        , createGame: const $
            Game.create pool cookies body
        , viewAllGames: const $
            Game.handleViewAll pool
        , viewGame: \{ handle } ->
            Game.handleView pool handle cookies
        , updateGame: \{ handle } ->
            Game.handleUpdate pool handle cookies body
        , createProfile: \identifiers ->
            Profile.create pool identifiers cookies body
        , updateProfile: \identifiers ->
            Profile.update pool identifiers cookies body
        , viewProfilesByGame: \{ handle, ilk, page, filters } ->
            Profile.viewByGame pool handle ilk page filters
        , viewProfilesByPlayer: \{ nickname, ilk } ->
            Profile.viewByPlayer pool nickname ilk
        , viewAllConversations: const $
            Conversation.viewAll pool cookies
        , viewConversation: \{ nickname } ->
            Conversation.view pool nickname cookies
        , startConversation: \{ nickname } ->
            Conversation.start pool client nickname cookies body
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
