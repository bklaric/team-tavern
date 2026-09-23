module TeamTavern.Server.Main where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Maybe.Trans (lift)
import Data.Either (either, note)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String as String
import Effect (Effect)
import Effect.Console (log)
import Jarilo.Serve (ServeOptions, serve)
import JavaScript.Node.Process (lookupEnv)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Pool as Pool
import TeamTavern.Routes.All (AllRoutes)
import TeamTavern.Server.Country.ViewCountries (viewCountries)
import TeamTavern.Server.Feed.ViewFeed (viewFeed)
import TeamTavern.Server.Feed.ViewOwnDescriptions (viewOwnDescriptions)
import TeamTavern.Server.Game.ViewGame (viewGame)
import TeamTavern.Server.Game.ViewGames (viewGames)
import TeamTavern.Server.Infrastructure.Deployment (Deployment)
import TeamTavern.Server.Infrastructure.Deployment as Deployment
import TeamTavern.Server.Infrastructure.FetchDiscordUser (DiscordApiUrl(..))
import TeamTavern.Server.Infrastructure.Log (logStamped, print)
import TeamTavern.Server.Infrastructure.Sendgrid (setApiKey)
import TeamTavern.Server.Password.ForgotPassword (forgotPassword)
import TeamTavern.Server.Password.ResetPassword (resetPassword)
import TeamTavern.Server.Player.ConfirmEmail (confirmEmail)
import TeamTavern.Server.Player.Register (register)
import TeamTavern.Server.Player.ResendConfirmation (resendConfirmation)
import TeamTavern.Server.Player.ViewMe (viewMe)
import TeamTavern.Server.Post.CreatePost (createPost)
import TeamTavern.Server.Post.DeletePost (deletePost)
import TeamTavern.Server.Post.UpdatePost (updatePost)
import TeamTavern.Server.Post.ViewOwnPost (viewOwnPost)
import TeamTavern.Server.Session.End (end) as Session
import TeamTavern.Server.Session.Start (start) as Session
import Type.Proxy (Proxy(..))

serveOptions :: ServeOptions { port :: Int, host :: String }
serveOptions =
    { listen: { port: 8080, host: "0.0.0.0" }
    , onRejected: \{ method, url, statusCode, reason } ->
        logStamped $ String.joinWith " | "
            ["Rejected request", show statusCode <> " " <> method <> " " <> url, reason]
    , onStreamError: \error ->
        logStamped $ "Request stream error | " <> print error
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

createPostgresPool :: ExceptT String Effect Pool
createPostgresPool = do
    postgresVariables <- loadPostgresVariables
    lift $ Pool.create postgresVariables

loadDeployment :: ExceptT String Effect Deployment
loadDeployment =
    lookupEnv "DEPLOYMENT"
    <#> bindFlipped Deployment.fromString
    <#> note "Couldn't read variable DEPLOYMENT."
    # ExceptT

loadDiscordApiUrl :: Effect DiscordApiUrl
loadDiscordApiUrl =
    lookupEnv "DISCORD_API_URL"
    <#> fromMaybe "https://discord.com/api"
    <#> DiscordApiUrl

runServer :: Deployment -> DiscordApiUrl -> Pool -> Effect Unit
runServer deployment discordApiUrl pool = serve (Proxy :: _ AllRoutes) serveOptions
    { startSession: \{ cookies, body } ->
        Session.start deployment discordApiUrl pool cookies body
    , endSession: \{ cookies } ->
        Session.end pool cookies
    , forgotPassword: \{ body } ->
        forgotPassword deployment pool body
    , resetPassword: \{ body } ->
        resetPassword pool body
    , registerPlayer: \{ cookies, body } ->
        register deployment discordApiUrl pool cookies body
    , viewMe: \{ cookies } ->
        viewMe deployment pool cookies
    , confirmEmail: \{ body } ->
        confirmEmail pool body
    , resendConfirmation: \{ cookies } ->
        resendConfirmation deployment pool cookies
    , viewGames: const $
        viewGames pool
    , viewGame: \{ path: { handle } } ->
        viewGame pool handle
    , viewFeed: \{ path: { handle }, cookies, body } ->
        viewFeed pool handle cookies body
    , viewOwnDescriptions: \{ path: { handle }, cookies } ->
        viewOwnDescriptions pool handle cookies
    , viewOwnPost: \{ path, cookies } ->
        viewOwnPost pool path.handle path.type cookies
    , createPost: \{ path, cookies, body } ->
        createPost pool path.handle path.type cookies body
    , updatePost: \{ path, cookies, body } ->
        updatePost pool path.handle path.type cookies body
    , deletePost: \{ path, cookies } ->
        deletePost pool path.handle path.type cookies
    , viewCountries: const $
        viewCountries pool
    }

main :: Effect Unit
main = either log pure =<< runExceptT do
    deployment <- loadDeployment
    discordApiUrl <- lift loadDiscordApiUrl
    pool <- createPostgresPool
    setSendGridApiKey
    lift $ runServer deployment discordApiUrl pool
