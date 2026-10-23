module TeamTavern.Server.Main where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Except (ExceptT(..), except, runExceptT)
import Control.Monad.Maybe.Trans (lift)
import Data.Array as Array
import Data.Either (either, note)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Effect (Effect)
import Jarilo.Serve (ServeOptions, serve)
import JavaScript.Node.Events.EventEmitter (on)
import JavaScript.Node.Events.EventListener (toEventListener)
import JavaScript.Node.Process (exit, lookupEnv)
import JavaScript.Npm.Pg.Client.Events as ClientEvents
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Pool as Pool
import JavaScript.Npm.Pg.Pool.Events as PoolEvents
import Record as Record
import TeamTavern.Routes.All (AllRoutes)
import TeamTavern.Server.Account.DeleteAccount (deleteAccount)
import TeamTavern.Server.Account.Infrastructure.SwitchSignIn (createSwitchLimit)
import TeamTavern.Server.Account.SwitchToDiscord (switchToDiscord)
import TeamTavern.Server.Account.SwitchToGoogle (switchToGoogle)
import TeamTavern.Server.Account.SwitchToPassword (switchToPassword)
import TeamTavern.Server.Account.SwitchToSteam (switchToSteam)
import TeamTavern.Server.Account.UpdateEmail (createUpdateEmailLimit, updateEmail)
import TeamTavern.Server.Account.UpdateFacts (updateFacts)
import TeamTavern.Server.Account.UpdateSwitches (updateSwitches)
import TeamTavern.Server.Account.ViewAccount (viewAccount)
import TeamTavern.Server.Block.Block (block)
import TeamTavern.Server.Block.Infrastructure.SendReportEmail (AdminEmail(..))
import TeamTavern.Server.Block.ReportConversation (reportConversation)
import TeamTavern.Server.Block.ReportPost (reportPost)
import TeamTavern.Server.Block.Unblock (unblock)
import TeamTavern.Server.Block.ViewBlocked (viewBlocked)
import TeamTavern.Server.ClientError.ReportClientError (ClientErrorLimits, createClientErrorLimits, reportClientError)
import TeamTavern.Server.Conversation.Infrastructure.PostMessage (createMessageLimit)
import TeamTavern.Server.Conversation.SendMessage (ConversationLimits, createConversationLimits, sendMessage)
import TeamTavern.Server.Conversation.SendReply (sendReply)
import TeamTavern.Server.Conversation.ViewConversation (viewConversation)
import TeamTavern.Server.Conversation.ViewInbox (viewInbox)
import TeamTavern.Server.Conversation.ViewPostConversation (viewPostConversation)
import TeamTavern.Server.Country.ViewCountries (viewCountries)
import TeamTavern.Server.Feed.ViewFeed (viewFeed)
import TeamTavern.Server.Feed.ViewOwnDescriptions (viewOwnDescriptions)
import TeamTavern.Server.Game.ViewGame (viewGame)
import TeamTavern.Server.Game.ViewGames (viewGames)
import TeamTavern.Server.Guide.ViewGuide (viewGuide)
import TeamTavern.Server.Guide.ViewGuides (viewGuides)
import TeamTavern.Server.Infrastructure.Bcrypt (Bcrypt, createBcrypt)
import TeamTavern.Server.Infrastructure.DiscordSignIn (DiscordClient(..), discordEndpoint)
import TeamTavern.Server.Infrastructure.Email (Mailer(..))
import TeamTavern.Server.Infrastructure.Environment (Environment(..))
import TeamTavern.Server.Infrastructure.Environment as Environment
import TeamTavern.Server.Infrastructure.GoogleSignIn (GoogleClient(..), googleEndpoint)
import TeamTavern.Server.Infrastructure.Log (logStamped, print)
import TeamTavern.Server.Infrastructure.Postgres (databaseErrorLines)
import TeamTavern.Server.Infrastructure.RateLimit (ClientLimits(..), Limiter, clientAddress)
import TeamTavern.Server.Infrastructure.RequestOrigin (requestOrigin)
import TeamTavern.Server.Infrastructure.ResolveSteamId (SteamApi(..))
import TeamTavern.Server.Infrastructure.Ses (createClient)
import TeamTavern.Server.Infrastructure.SteamOpenId (SteamOpenIdUrl(..), steamEndpoint)
import TeamTavern.Server.LlmsTxt.ViewLlmsTxt (viewLlmsTxt)
import TeamTavern.Server.Notification.ReadNotification (readNotification)
import TeamTavern.Server.Notification.ReadNotifications (readNotifications)
import TeamTavern.Server.Notification.ViewNotifications (viewNotifications)
import TeamTavern.Server.Password.ForgotPassword (ForgotPasswordLimits, createForgotPasswordLimits, forgotPassword)
import TeamTavern.Server.Password.ResetPassword (resetPassword)
import TeamTavern.Server.Player.ConfirmEmail (confirmEmail)
import TeamTavern.Server.Player.Infrastructure.SendConfirmation (ConfirmationLimits, createConfirmationLimits)
import TeamTavern.Server.Player.Register (createRegisterLimit, register)
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
import TeamTavern.Server.Session.Start (createProviderSignInLimit, start) as Session
import TeamTavern.Server.Session.Start.CheckPassword (SignInLimits, createSignInLimits)
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

-- Postgres ending a connection, as a restart does, makes its client emit an
-- error, and an error nobody listens for ends the process. The pool listens
-- only to a client it holds idle, and emits the error again on itself, so
-- each client gets a listener of its own, which also covers a client checked
-- out for a transaction, and the pool's error needs one that does nothing.
-- A client that lost its connection fails every query after, and the pool
-- drops it when it is handed back.
--
-- The slowest queries, a feed batch that reaches the expired posts of the
-- biggest game and the fits of a new post there, take about a second at most
-- on production's data. So Postgres cancels a statement still running, or
-- waiting on a lock, after ten seconds, and the request fails and hands its
-- connection back. Postgres enforces it itself, so the connection stays usable
-- and a transaction around the statement rolls back. Nothing awaits anything
-- but Postgres inside a transaction, so one left idle for ten seconds is stuck,
-- and Postgres ends its connection and so its locks. A request that waits five
-- seconds for one of the pool's ten connections fails rather than queue with
-- no end.
createPostgresPool :: ExceptT String Effect Pool
createPostgresPool = do
    postgresVariables <- loadPostgresVariables
    pool <- lift $ Pool.create $ Record.union postgresVariables
        { max: 10
        , connectionTimeoutMillis: 5000
        , statement_timeout: 10000
        , idle_in_transaction_session_timeout: 10000
        }
    lift $ pool # on PoolEvents.connect (toEventListener \client ->
        client # on ClientEvents.error (toEventListener \error ->
            logStamped $ String.joinWith " | " $ Array.cons "Postgres connection lost" $ databaseErrorLines error)
        # void)
        # void
    lift $ pool # on PoolEvents.error (toEventListener \_ _ -> pure unit) # void
    pure pool

loadEnvironment :: ExceptT String Effect Environment
loadEnvironment =
    lookupEnv "ENVIRONMENT"
    <#> bindFlipped Environment.fromString
    <#> note "Couldn't read variable ENVIRONMENT."
    # ExceptT

-- | Discord's API unless DISCORD_API_URL names another, as the local stack's
-- | does.
loadDiscordClient :: ExceptT String Effect DiscordClient
loadDiscordClient = do
    secret <- lookupEnv "DISCORD_CLIENT_SECRET"
        <#> note "Couldn't read variable DISCORD_CLIENT_SECRET." # ExceptT
    apiUrl <- lift $ lookupEnv "DISCORD_API_URL" <#> fromMaybe discordEndpoint
    pure $ DiscordClient { apiUrl, secret }

-- | Steam's Web API at `api.steampowered.com` unless STEAM_API_URL names
-- | another, as the local stack's does.
loadSteamApi :: ExceptT String Effect SteamApi
loadSteamApi = do
    key <- lookupEnv "STEAM_API_KEY"
        <#> note "Couldn't read variable STEAM_API_KEY." # ExceptT
    url <- lift $ lookupEnv "STEAM_API_URL" <#> fromMaybe "https://api.steampowered.com"
    pure $ SteamApi { url, key }

-- | Steam's OpenID provider at `steamcommunity.com` unless STEAM_OPENID_URL
-- | names another, as the local stack's does.
loadSteamOpenIdUrl :: Effect SteamOpenIdUrl
loadSteamOpenIdUrl =
    lookupEnv "STEAM_OPENID_URL"
    <#> fromMaybe steamEndpoint
    <#> SteamOpenIdUrl

-- | Google's token endpoint unless GOOGLE_TOKEN_URL names another, as the local
-- | stack's does.
loadGoogleClient :: ExceptT String Effect GoogleClient
loadGoogleClient = do
    secret <- lookupEnv "GOOGLE_CLIENT_SECRET"
        <#> note "Couldn't read variable GOOGLE_CLIENT_SECRET." # ExceptT
    tokenUrl <- lift $ lookupEnv "GOOGLE_TOKEN_URL" <#> fromMaybe googleEndpoint
    pure $ GoogleClient { tokenUrl, secret }

-- | Staging and production send through SES and link to their own origin. The
-- | local stack links relative to the site it serves, and only logs, unless
-- | AWS_ENDPOINT_URL_SESV2 names something that takes SES's requests, as its
-- | mail stub does. The SDK reads that variable itself.
loadMailer :: Environment -> ExceptT String Effect Mailer
loadMailer environment = do
    accessKeyId <- lookupEnv "AWS_ACCESS_KEY_ID"
        <#> note "Couldn't read variable AWS_ACCESS_KEY_ID." # ExceptT
    secretAccessKey <- lookupEnv "AWS_SECRET_ACCESS_KEY"
        <#> note "Couldn't read variable AWS_SECRET_ACCESS_KEY." # ExceptT
    endpoint <- lift $ lookupEnv "AWS_ENDPOINT_URL_SESV2"
    client <- lift $ createClient { accessKeyId, secretAccessKey }
    pure $ Mailer case environment of
        Production -> { origin: "https://www.teamtavern.net", client: Just client }
        Staging -> { origin: "https://staging.teamtavern.net", client: Just client }
        Local -> { origin: "", client: endpoint $> client }

-- | The worker's period in seconds, an hour unless WORKER_PERIOD says otherwise,
-- | as the local stack's does.
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

-- | The limits kept per client address, unless RATE_LIMIT_CLIENTS is `off`, as
-- | the local stack's is.
loadClientLimits :: Effect ClientLimits
loadClientLimits = lookupEnv "RATE_LIMIT_CLIENTS" <#> (_ /= Just "off") <#> ClientLimits

type Limits =
    { clientErrors :: ClientErrorLimits
    , forgotPassword :: ForgotPasswordLimits
    , confirmation :: ConfirmationLimits
    , updateEmail :: Limiter
    , signIn :: SignInLimits
    , providerSignIn :: Limiter
    , switchSignIn :: Limiter
    , register :: Limiter
    , conversations :: ConversationLimits
    , messages :: Limiter
    }

createLimits :: ClientLimits -> Effect Limits
createLimits clientLimits = do
    clientErrors <- createClientErrorLimits clientLimits
    forgotPassword <- createForgotPasswordLimits clientLimits
    confirmation <- createConfirmationLimits
    updateEmail <- createUpdateEmailLimit
    signIn <- createSignInLimits clientLimits
    providerSignIn <- Session.createProviderSignInLimit clientLimits
    switchSignIn <- createSwitchLimit
    register <- createRegisterLimit clientLimits
    conversations <- createConversationLimits
    messages <- createMessageLimit
    pure { clientErrors, forgotPassword, confirmation, updateEmail, signIn, providerSignIn, switchSignIn, register, conversations, messages }

runServer :: Environment -> Mailer -> DiscordClient -> SteamApi -> SteamOpenIdUrl -> GoogleClient -> AdminEmail -> Limits -> Bcrypt -> Pool -> Effect Unit
runServer environment mailer discordClient steamApi steamOpenIdUrl googleClient adminEmail limits bcrypt pool = serve (Proxy :: _ AllRoutes) serveOptions
    { startSession: \{ cookies, headers, body } ->
        Session.start environment limits.signIn limits.providerSignIn limits.confirmation bcrypt mailer discordClient steamOpenIdUrl steamApi googleClient
            pool cookies (clientAddress headers) (requestOrigin headers) body
    , endSession: \{ cookies } ->
        Session.end pool cookies
    , forgotPassword: \{ headers, body } ->
        forgotPassword limits.forgotPassword mailer pool headers body
    , resetPassword: \{ body } ->
        resetPassword limits.signIn bcrypt pool body
    , registerPlayer: \{ cookies, headers, body } ->
        register environment limits.register limits.confirmation bcrypt mailer pool cookies (clientAddress headers) body
    , viewMe: \{ cookies } ->
        viewMe environment pool cookies
    , confirmEmail: \{ body } ->
        confirmEmail pool body
    , resendConfirmation: \{ cookies } ->
        resendConfirmation limits.confirmation mailer pool cookies
    , viewAccount: \{ cookies } ->
        viewAccount pool cookies
    , updateFacts: \{ cookies, body } ->
        updateFacts steamApi pool cookies body
    , updateSwitches: \{ cookies, body } ->
        updateSwitches pool cookies body
    , updateEmail: \{ cookies, body } ->
        updateEmail limits.updateEmail limits.confirmation mailer pool cookies body
    , switchToDiscord: \{ cookies, headers, body } ->
        switchToDiscord limits.switchSignIn discordClient pool cookies (requestOrigin headers) body
    , switchToSteam: \{ cookies, headers, body } ->
        switchToSteam limits.switchSignIn steamOpenIdUrl pool cookies (requestOrigin headers) body
    , switchToGoogle: \{ cookies, headers, body } ->
        switchToGoogle limits.switchSignIn googleClient pool cookies (requestOrigin headers) body
    , switchToPassword: \{ cookies, body } ->
        switchToPassword limits.confirmation bcrypt mailer pool cookies body
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
        createPost steamApi pool path.handle path.type cookies body
    , updatePost: \{ path, cookies, body } ->
        updatePost steamApi pool path.handle path.type cookies body
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
        sendMessage limits.messages limits.conversations mailer pool path.handle path.id cookies body
    , sendReply: \{ path: { id }, cookies, body } ->
        sendReply limits.messages mailer pool id cookies body
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
    , viewGuides: const
        viewGuides
    , viewGuide: \{ path: { slug } } ->
        viewGuide slug
    , viewSitemap: \{ headers } ->
        viewSitemap pool headers
    , viewLlmsTxt: \{ headers } ->
        viewLlmsTxt pool headers
    , reportClientError: \{ headers, body } ->
        reportClientError limits.clientErrors headers body
    }

main :: Effect Unit
main = either (\error -> logStamped error *> exit 1) pure =<< runExceptT do
    environment <- loadEnvironment
    discordClient <- loadDiscordClient
    steamApi <- loadSteamApi
    steamOpenIdUrl <- lift loadSteamOpenIdUrl
    googleClient <- loadGoogleClient
    adminEmail <- loadAdminEmail
    workerPeriod <- loadWorkerPeriod
    pool <- createPostgresPool
    mailer <- loadMailer environment
    clientLimits <- lift loadClientLimits
    limits <- lift $ createLimits clientLimits
    bcrypt <- lift createBcrypt
    lift $ startWorker workerPeriod mailer pool
    lift $ runServer environment mailer discordClient steamApi steamOpenIdUrl googleClient adminEmail limits bcrypt pool
