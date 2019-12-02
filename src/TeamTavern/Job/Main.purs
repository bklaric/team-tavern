module TeamTavern.Job.Main (main) where

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
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Class.Console (log)
import Foreign (MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Async.Query (query_)
import Postgres.Error (Error)
import Postgres.Query (class Querier, Query(..))
import Postgres.Result (Result, rows)
import Postmark.Async.Client (sendEmail)
import Postmark.Client (Client)
import Simple.JSON.Async (read)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Main (createPostgresPool, createPostmarkClient, loadDeployment)

type NotifyError = Variant
    ( databaseError :: Error
    , unreadablePlayers ::
        { errors :: MultipleErrors
        , result :: Result
        }
    )

updateNotifiedPlayersQuery :: Query
updateNotifiedPlayersQuery = Query """
    update player
    set create_profile_notified = now()
    where id in (
        select
            player.id
        from player
            left join profile on profile.player_id = player.id
        where
            player.email_confirmed
            and player.registered < (now() - interval '1 day')
            and player.create_profile_notified is null
            and profile.id is null
    )
    returning
        player.nickname,
        player.email
   """

createProfileNotify :: forall querier. Querier querier =>
    querier -> Maybe Client -> Async NotifyError Unit
createProfileNotify pool client = do
    -- Load users to be notified.
    result <- query_ updateNotifiedPlayersQuery pool
        # label (SProxy :: SProxy "databaseError")
    players :: Array { nickname :: String, email :: String } <- rows result
        # traverse read
        # labelMap (SProxy :: SProxy "unreadablePlayers") { result, errors: _ }

    log "Sending email to the following players: "
    foreach players \{ nickname, email } ->
        log $ nickname <> " (" <> email <> ")"

    -- Create a mesage for each user.
    let messages = players <#> \{ nickname, email } ->
            { nickname
            , email
            , message:
                { to: email
                , from: "TeamTavern admin@teamtavern.net"
                , subject: notNull "Is everything alright?"
                , htmlBody: notNull $
                    "Hi " <> nickname <> ",<br /><br />"
                    <> "You have successfully registered to TeamTavern, "
                    <> "but haven't created a profile yet. "
                    <> "Are you having trouble setting up your profile?<br /><br />"
                    <> "Feel free to reply to this email if you want to share "
                    <> "anything about your experience of using TeamTavern "
                    <> "or if you have any questions.<br /><br />"
                    <> "Best regards,<br />"
                    <> "TeamTavern admin"
                , textBody: null
                }
            }

    -- Send out the emails.
    case client of
        Just client' -> do
            sendEmailResult <-
                messages
                <#> (\{ nickname, email, message} ->
                    sendEmail message client'
                    # attempt
                    <#> { nickname, email, result: _ })
                <#> parallel
                # traverse sequential
            foreach sendEmailResult case _ of
                { nickname, email, result: Left error } -> do
                    log $ "Error sending email to " <> nickname <> " (" <> email <> ")"
                    log $ "This is the error: " <> show error
                { nickname, email, result: Right unit } -> pure unit
        Nothing -> log "Dry run, nothing sent."

runNotify :: forall querier. Querier querier =>
    querier -> Maybe Client -> Effect Unit
runNotify pool client =
    createProfileNotify pool client
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
