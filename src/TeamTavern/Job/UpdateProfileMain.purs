module TeamTavern.Job.UpdateProfileMain (main) where

import Prelude

import Async (Async, attempt, foreach, runAsync)
import Control.Monad.Except (lift, runExceptT)
import Control.Parallel (parallel, sequential)
import Data.Bifunctor.Label (label, labelMap)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull, null)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Variant (Variant, inj, match)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Async.Pool (withTransaction)
import Postgres.Async.Query (execute, query_)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), (:))
import Postgres.Result (Result, rows)
import Postmark.Async.Client (sendEmail)
import Postmark.Client (Client)
import Simple.JSON.Async (read)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Main (createPostgresPool, createPostmarkClient, loadDeployment)

type Player =
    { id :: Int
    , nickname :: String
    , email :: String
    , ilk :: Int
    , title :: String
    }

type NotifyError = Variant
    ( databaseError :: Error
    , unreadablePlayers ::
        { errors :: MultipleErrors
        , result :: Result
        }
    )

loadPlayersToNotifyQuery :: Query
loadPlayersToNotifyQuery = Query """
    select
        player.id,
        player.nickname,
        player.email,
        profile.type as ilk,
        game.title
    from player
        join profile on profile.player_id = player.id
        join game on game.id = profile.game_id
    where
        player.email_confirmed
        and player.update_profile_notified is null
        and profile.updated < (now() - interval '3 month')
    order by
        player.id
    """

updateNotifiedPlayersQuery :: Query
updateNotifiedPlayersQuery = Query """
    update player
    set update_profile_notified = now()
    where id = any($1)
    """

updateProfileNotify :: Pool -> Maybe Client -> Async NotifyError Unit
updateProfileNotify pool postmarkClient = do
    players <- pool # withTransaction (inj (SProxy :: SProxy "databaseError")) (\postgresClient -> do
        -- Load users to be notified.
        result <- query_ loadPlayersToNotifyQuery postgresClient
            # label (SProxy :: SProxy "databaseError")
        players :: Array Player <- rows result
            # traverse read
            # labelMap (SProxy :: SProxy "unreadablePlayers") { result, errors: _ }

        -- Record that players have been notified.
        execute updateNotifiedPlayersQuery ((players <#> _.id) : []) postgresClient
            # label (SProxy :: SProxy "databaseError")

        pure players
    )

    log "Sending email to the following players: "
    foreach players \{ id, nickname, email } ->
        log $ show id <> " " <> nickname <> " (" <> email <> ")"

    -- Create a mesage for each user.
    let messages = players <#> \{ id, nickname, email, ilk, title } ->
            { id
            , nickname
            , email
            , message:
                { to: email
                , from: "TeamTavern admin@teamtavern.net"
                , subject: notNull $ "Update your " <> title <> " " <> (if ilk == 0 then "player" else "team") <> " profile"
                , htmlBody: notNull $
                    "Hi " <> nickname <> ",<br /><br />"
                    <> "It has been a while since you've last updated your " <> title <> " " <> (if ilk == 0 then "player" else "team") <> " profile on TeamTavern. "
                    <> "Is it still up to date?. "
                    <> "<a href='https://www.teamtavern.net/signin'>Sign in to TeamTavern</a> "
                    <> "and update your " <> title <> " profile so your future teammates can find you.<br /><br />"
                    <> "Happy playing!"
                , textBody: null
                }
            }

    -- Send out the emails.
    case postmarkClient of
        Just postmarkClient' -> do
            sendEmailResult <-
                messages
                <#> (\{ id, nickname, email, message } ->
                    sendEmail message postmarkClient'
                    # attempt
                    <#> { id, nickname, email, result: _ })
                <#> parallel
                # traverse sequential
            foreach sendEmailResult case _ of
                { id, nickname, email, result: Left error } -> do
                    log $ "Error sending email to " <> show id <> " " <> nickname <> " (" <> email <> ")"
                    log $ "This is the error: " <> show error
                { result: Right unit } -> pure unit
        Nothing -> log "Dry run, nothing sent."

runNotify :: Pool -> Maybe Client -> Effect Unit
runNotify pool client =
    updateProfileNotify pool client
    # runAsync case _ of
        Left notifyError -> notifyError # match
            { databaseError: \databaseError ->
                log $ "Unexpected database error occurred: " <> print databaseError
            , unreadablePlayers: \{ result, errors } -> do
                log $ "Couldn't read players from result: " <> (unsafeStringify $ rows result)
                log $ "Reading resulted in these errors: " <> show errors
            }
        Right unit -> log "Everything went well!"

main :: Effect Unit
main = either log pure =<< runExceptT do
    deployment <- loadDeployment
    pool <- createPostgresPool
    client <- createPostmarkClient deployment
    lift $ runNotify pool client
