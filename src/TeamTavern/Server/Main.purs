module TeamTavern.Server.Main where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Except (ExceptT(..), except, runExceptT)
import Control.Monad.Maybe.Trans (lift)
import Data.Either (either, note)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect (Effect)
import Effect.Console (log)
import Jarilo.Serve (ServeOptions, serve)
import JavaScript.Node.Process (lookupEnv)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Pool as Pool
import TeamTavern.Routes.All (AllRoutes)
import TeamTavern.Server.Account.DeleteAccount (deleteAccount)
import TeamTavern.Server.Account.SwitchToDiscord (switchToDiscord)
import TeamTavern.Server.Account.SwitchToPassword (switchToPassword)
import TeamTavern.Server.Account.UpdateEmail (updateEmail)
import TeamTavern.Server.Account.UpdateFacts (updateFacts)
import TeamTavern.Server.Account.UpdateSwitches (updateSwitches)
import TeamTavern.Server.Account.ViewAccount (viewAccount)
import TeamTavern.Server.Block.Block (block)
import TeamTavern.Server.Block.Infrastructure.SendReportEmail (AdminEmail(..))
import TeamTavern.Server.Block.ReportConversation (reportConversation)
import TeamTavern.Server.Block.ReportPost (reportPost)
import TeamTavern.Server.Block.Unblock (unblock)
import TeamTavern.Server.Block.ViewBlocked (viewBlocked)
import TeamTavern.Server.Conversation.SendMessage (sendMessage)
import TeamTavern.Server.Conversation.SendReply (sendReply)
import TeamTavern.Server.Conversation.ViewConversation (viewConversation)
import TeamTavern.Server.Conversation.ViewInbox (viewInbox)
import TeamTavern.Server.Conversation.ViewPostConversation (viewPostConversation)
import TeamTavern.Server.Country.ViewCountries (viewCountries)
import TeamTavern.Server.Feed.ViewFeed (viewFeed)
import TeamTavern.Server.Feed.ViewOwnDescriptions (viewOwnDescriptions)
import TeamTavern.Server.Game.ViewGame (viewGame)
import TeamTavern.Server.Game.ViewGames (viewGames)
import TeamTavern.Server.Infrastructure.Deployment (Deployment(..))
import TeamTavern.Server.Infrastructure.Deployment as Deployment
import TeamTavern.Server.Infrastructure.Email (Mailer(..))
import TeamTavern.Server.Infrastructure.FetchDiscordUser (DiscordApiUrl(..))
import TeamTavern.Server.Infrastructure.Log (logStamped, print)
import TeamTavern.Server.Infrastructure.Sendgrid (setApiKey, setBaseUrl)
import TeamTavern.Server.Notification.ReadNotification (readNotification)
import TeamTavern.Server.Notification.ReadNotifications (readNotifications)
import TeamTavern.Server.Notification.ViewNotifications (viewNotifications)
import TeamTavern.Server.Password.ForgotPassword (forgotPassword)
import TeamTavern.Server.Password.ResetPassword (resetPassword)
import TeamTavern.Server.Player.ConfirmEmail (confirmEmail)
import TeamTavern.Server.Player.Register (register)
import TeamTavern.Server.Player.ResendConfirmation (resendConfirmation)
import TeamTavern.Server.Player.ViewMe (viewMe)
import TeamTavern.Server.Post.CreatePost (createPost)
import TeamTavern.Server.Post.DeletePost (deletePost)
import TeamTavern.Server.Post.RenewByNonce (renewByNonce)
import TeamTavern.Server.Post.RenewPost (renewPost)
import TeamTavern.Server.Post.RevealContacts (revealContacts)
import TeamTavern.Server.Post.UpdatePost (updatePost)
import TeamTavern.Server.Post.ViewOwnPost (viewOwnPost)
import TeamTavern.Server.Post.ViewOwnPosts (viewOwnPosts)
import TeamTavern.Server.Post.ViewPost (viewPost)
import TeamTavern.Server.Session.End (end) as Session
import TeamTavern.Server.Session.Start (start) as Session
import TeamTavern.Server.Sitemap.ViewSitemap (viewSitemap)
import TeamTavern.Server.Worker (startWorker)
import Type.Proxy (Proxy(..))

serveOptions :: ServeOptions { port :: Int, host :: String }
serveOptions =
    { listen: { port: 80, host: "0.0.0.0" }
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

-- | Production sends through SendGrid and links to its own origin. The local
-- | stacks link relative to the site they serve, and only log, unless
-- | SENDGRID_API_URL names something that takes SendGrid's requests, as the test
-- | stack's mail stub does. The key is set by then, since setting it resets the
-- | base URL.
loadMailer :: Deployment -> Effect Mailer
loadMailer deployment = do
    apiUrl <- lookupEnv "SENDGRID_API_URL"
    case apiUrl of
        Just url -> setBaseUrl url
        Nothing -> pure unit
    pure $ Mailer case deployment, apiUrl of
        Cloud, _ -> { origin: "https://www.teamtavern.net", send: true }
        Local, Just _ -> { origin: "", send: true }
        Local, Nothing -> { origin: "", send: false }

-- | The worker's period in seconds, an hour unless WORKER_PERIOD says otherwise,
-- | as the test stack's does.
loadWorkerPeriod :: ExceptT String Effect Int
loadWorkerPeriod = do
    period <- lift $ lookupEnv "WORKER_PERIOD"
    case period of
        Nothing -> pure 3600
        Just string -> fromString string # note "Couldn't read variable WORKER_PERIOD." # except

loadAdminEmail :: ExceptT String Effect AdminEmail
loadAdminEmail =
    lookupEnv "ADMIN_EMAIL"
    <#> map AdminEmail
    <#> note "Couldn't read variable ADMIN_EMAIL."
    # ExceptT

runServer :: Deployment -> Mailer -> DiscordApiUrl -> AdminEmail -> Pool -> Effect Unit
runServer deployment mailer discordApiUrl adminEmail pool = serve (Proxy :: _ AllRoutes) serveOptions
    { startSession: \{ cookies, body } ->
        Session.start deployment mailer discordApiUrl pool cookies body
    , endSession: \{ cookies } ->
        Session.end pool cookies
    , forgotPassword: \{ body } ->
        forgotPassword mailer pool body
    , resetPassword: \{ body } ->
        resetPassword pool body
    , registerPlayer: \{ cookies, body } ->
        register deployment mailer discordApiUrl pool cookies body
    , viewMe: \{ cookies } ->
        viewMe deployment pool cookies
    , confirmEmail: \{ body } ->
        confirmEmail pool body
    , resendConfirmation: \{ cookies } ->
        resendConfirmation mailer pool cookies
    , viewAccount: \{ cookies } ->
        viewAccount pool cookies
    , updateFacts: \{ cookies, body } ->
        updateFacts pool cookies body
    , updateSwitches: \{ cookies, body } ->
        updateSwitches pool cookies body
    , updateEmail: \{ cookies, body } ->
        updateEmail mailer pool cookies body
    , switchToDiscord: \{ cookies, body } ->
        switchToDiscord discordApiUrl pool cookies body
    , switchToPassword: \{ cookies, body } ->
        switchToPassword mailer pool cookies body
    , deleteAccount: \{ cookies } ->
        deleteAccount pool cookies
    , viewGames: const $
        viewGames pool
    , viewGame: \{ path: { handle } } ->
        viewGame pool handle
    , viewFeed: \{ path: { handle }, cookies, body } ->
        viewFeed pool handle cookies body
    , viewOwnDescriptions: \{ path: { handle }, cookies } ->
        viewOwnDescriptions pool handle cookies
    , viewPost: \{ path, cookies } ->
        viewPost pool path.handle path.id cookies
    , viewOwnPosts: \{ cookies } ->
        viewOwnPosts pool cookies
    , viewOwnPost: \{ path, cookies } ->
        viewOwnPost pool path.handle path.type cookies
    , createPost: \{ path, cookies, body } ->
        createPost pool path.handle path.type cookies body
    , updatePost: \{ path, cookies, body } ->
        updatePost pool path.handle path.type cookies body
    , renewPost: \{ path, cookies } ->
        renewPost pool path.handle path.id cookies
    , renewByNonce: \{ body } ->
        renewByNonce pool body
    , revealContacts: \{ path, cookies } ->
        revealContacts pool path.handle path.id cookies
    , viewInbox: \{ cookies } ->
        viewInbox pool cookies
    , viewConversation: \{ path: { id }, cookies } ->
        viewConversation pool id cookies
    , viewPostConversation: \{ path, cookies } ->
        viewPostConversation pool path.handle path.id cookies
    , sendMessage: \{ path, cookies, body } ->
        sendMessage mailer pool path.handle path.id cookies body
    , sendReply: \{ path: { id }, cookies, body } ->
        sendReply mailer pool id cookies body
    , block: \{ path: { nickname }, cookies } ->
        block pool nickname cookies
    , unblock: \{ path: { nickname }, cookies } ->
        unblock pool nickname cookies
    , viewBlocked: \{ cookies } ->
        viewBlocked pool cookies
    , reportPost: \{ path, cookies, body } ->
        reportPost mailer adminEmail pool path.handle path.id cookies body
    , reportConversation: \{ path: { id }, cookies, body } ->
        reportConversation mailer adminEmail pool id cookies body
    , viewNotifications: \{ cookies } ->
        viewNotifications pool cookies
    , readNotifications: \{ cookies } ->
        readNotifications pool cookies
    , readNotification: \{ path: { id }, cookies } ->
        readNotification pool id cookies
    , deletePost: \{ path, cookies } ->
        deletePost pool path.handle path.type cookies
    , viewCountries: const $
        viewCountries pool
    , viewSitemap: \{ headers } ->
        viewSitemap pool headers
    }

main :: Effect Unit
main = either log pure =<< runExceptT do
    deployment <- loadDeployment
    discordApiUrl <- lift loadDiscordApiUrl
    adminEmail <- loadAdminEmail
    workerPeriod <- loadWorkerPeriod
    pool <- createPostgresPool
    setSendGridApiKey
    mailer <- lift $ loadMailer deployment
    lift $ startWorker workerPeriod mailer pool
    lift $ runServer deployment mailer discordApiUrl adminEmail pool
