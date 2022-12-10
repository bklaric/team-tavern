module TeamTavern.Server.Main where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Maybe.Trans (lift)
import Data.Either (either, note)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Options (Options, (:=))
import Effect (Effect)
import Effect.Console (log)
import Jarilo.Serve (serve)
import Node.Process (lookupEnv)
import Node.Server (ListenOptions(..))
import Postgres.Client.Config (ClientConfig, database, host, password, port, user)
import Postgres.Pool (Pool)
import Postgres.Pool as Pool
import Sendgrid (setApiKey)
import TeamTavern.Routes.All (AllRoutes)
import TeamTavern.Routes.Profile.ViewPlayerProfilesByGame (bundlePlayerFilters)
import TeamTavern.Routes.Profile.ViewTeamProfilesByGame (bundleTeamFilters)
import TeamTavern.Server.Alert.Create (createAlert) as Alert
import TeamTavern.Server.Alert.Delete (deleteAlert) as Alert
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.Deployment as Deployment
import TeamTavern.Server.Boarding.Onboard as Onboard
import TeamTavern.Server.Boarding.Preboard as Preboard
import TeamTavern.Server.Game.View (view) as Game
import TeamTavern.Server.Game.ViewAll (viewAll) as Game
import TeamTavern.Server.Player.Delete (delete) as Player
import TeamTavern.Server.Player.Register (register) as Player
import TeamTavern.Server.Player.UpdateContacts (updateContacts) as Player
import TeamTavern.Server.Player.UpdatePlayer (updatePlayer) as Player
import TeamTavern.Server.Player.View (view) as Player
import TeamTavern.Server.Profile.AddPlayerProfile (addPlayerProfile) as Profile
import TeamTavern.Server.Profile.AddTeamProfile (addTeamProfile) as Profile
import TeamTavern.Server.Profile.UpdatePlayerProfile (updatePlayerProfile) as Profile
import TeamTavern.Server.Profile.UpdateTeamProfile (updateTeamProfile) as Profile
import TeamTavern.Server.Profile.ViewPlayerProfile (viewPlayerProfile) as Profile
import TeamTavern.Server.Profile.ViewPlayerProfilesByGame (viewPlayerProfilesByGame) as Profile
import TeamTavern.Server.Profile.ViewTeamProfile (viewTeamProfile) as Profile
import TeamTavern.Server.Profile.ViewTeamProfilesByGame (viewTeamProfilesByGame) as Profile
import TeamTavern.Server.Session.End (end) as Session
import TeamTavern.Server.Session.Start (start) as Session
import TeamTavern.Server.Team.Create (create) as Team
import TeamTavern.Server.Team.Update (update) as Team
import TeamTavern.Server.Team.UpdateContacts (updateContacts) as Team
import TeamTavern.Server.Team.View (view) as Team
import Type.Proxy (Proxy(..))

listenOptions :: ListenOptions
listenOptions = TcpListenOptions
    { port: Just 8080
    , host: Just "0.0.0.0"
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

teamTavernRoutes = Proxy :: Proxy AllRoutes

runServer :: Deployment -> Pool -> Effect Unit
runServer deployment pool = serve (Proxy :: _ AllRoutes) listenOptions
    { startSession: \{ cookies, body } ->
        Session.start deployment pool cookies body
    , endSession: const
        Session.end
    , viewAllGames: const $
        Game.viewAll pool
    , viewGame: \{ path: { handle } } ->
        Game.view pool handle
    , viewPlayer: \{ path: { nickname } , query: { timezone }, cookies } ->
        Player.view pool cookies { nickname, timezone }
    , registerPlayer: \{ cookies, body } ->
        Player.register deployment pool cookies body
    , updatePlayer: \{ path, cookies, body } ->
        Player.updatePlayer pool path.nickname cookies body
    , deletePlayer: \{ path, cookies } ->
        Player.delete pool path.nickname cookies
    , updatePlayerContacts: \{ path, cookies, body } ->
        Player.updateContacts pool path.nickname cookies body
    , viewTeam: \{ path: { handle }, query: { timezone } } ->
        Team.view pool { handle, timezone }
    , createTeam: \{ cookies, body } ->
        Team.create pool cookies body
    , updateTeam: \{ path, cookies, body } ->
        Team.update pool cookies path body
    , updateTeamContacts: \{ path, cookies, body } ->
        Team.updateContacts pool cookies path body
    , addPlayerProfile: \{ path, cookies, body } ->
        Profile.addPlayerProfile pool cookies path body
    , addTeamProfile: \{ path, cookies, body } ->
        Profile.addTeamProfile pool cookies path body
    , updatePlayerProfile: \{ path, cookies, body } ->
        Profile.updatePlayerProfile pool cookies path body
    , updateTeamProfile: \{ path, cookies, body } ->
        Profile.updateTeamProfile pool cookies path body
    , viewPlayerProfilesByGame: \{ path: { handle }, query } ->
        Profile.viewPlayerProfilesByGame pool handle query.page query.timezone $ bundlePlayerFilters query
    , viewTeamProfilesByGame: \{ path: { handle }, query } ->
        Profile.viewTeamProfilesByGame pool handle query.page query.timezone $ bundleTeamFilters query
    , viewPlayerProfile: \{ path: { nickname, handle }, query: { timezone } } ->
        Profile.viewPlayerProfile pool { nickname, handle, timezone }
    , viewTeamProfile: \{ path: { teamHandle, gameHandle }, query: { timezone } } ->
        Profile.viewTeamProfile pool { teamHandle, gameHandle, timezone }
    , onboard: \{ cookies, body } ->
        Onboard.onboard pool cookies body
    , preboard: \{ cookies, body } ->
        Preboard.preboard deployment pool cookies body
    , createAlert: \{ body } ->
        Alert.createAlert pool body
    , deleteAlert: \{ path: { id }, query: { token } } ->
        Alert.deleteAlert pool { id, token }
    }

main :: Effect Unit
main = either log pure =<< runExceptT do
    deployment <- loadDeployment
    pool <- createPostgresPool
    setSendGridApiKey
    lift $ runServer deployment pool
