module TeamTavern.RelaunchEmail.Main (main) where

import Prelude

import Async (Async, alwaysRightWithEffect, attempt, fromEffect, runSafeAsync)
import Control.Monad.Except (runExceptT)
import Control.Monad.Maybe.Trans (lift)
import Data.Array (length)
import Data.Either (Either(..), either)
import Data.Foldable (sum)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import JavaScript.Node.Process (exit)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..))
import TeamTavern.Server.Infrastructure.Email (Block(..), Email, Mailer, deliver)
import TeamTavern.Server.Infrastructure.GenerateNonce (toString)
import TeamTavern.Server.Infrastructure.Log (logError, logStamped)
import TeamTavern.Server.Infrastructure.Postgres (queryMany_)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Main (createPostgresPool, loadEnvironment, loadMailer, setSendGridApiKey)
import TeamTavern.Server.Player.Infrastructure.SendConfirmation (Confirmation, addConfirmation)

-- | The one email of the relaunch (brief 12). Every player the import brought
-- | over who was seen in the year before it, and whose address nobody has
-- | confirmed, is told what changed and sent the link that confirms the
-- | address, since until then the site sends it nothing else. It runs in the
-- | node container, configured as the server is:
-- |
-- |     docker exec tt-node node /root/team-tavern/server/relaunch-email.js [--send]
-- |
-- | Without --send it only counts. The link is a confirmation row, and a player
-- | who has one isn't sent to, so a run cut short picks up where it stopped.
foreign import sending :: Effect Boolean

type Recipient = { id :: Int, nickname :: String, email :: String }

-- Seen: registered, signed in or updated a post. The import keeps when each
-- session was made, not when it was last used.
recipientsQuery :: Query
recipientsQuery = Query """
    select player.id, player.nickname, player.email
    from player
    where player.email is not null
        and not player.email_confirmed
        and not exists
            (select 1 from email_confirmation where email_confirmation.player_id = player.id)
        and greatest(
            player.registered,
            (select max(session.generated) from session where session.player_id = player.id),
            (select max(post.updated) from post where post.player_id = player.id)
        ) > now() - interval '1 year'
    order by player.id
    """

relaunchEmail :: Confirmation -> Email
relaunchEmail { email, nickname, nonce } =
    { to: email
    , subject: "TeamTavern has relaunched"
    , blocks:
        [ Paragraph $ "Hi " <> nickname <> ","
        , Paragraph $ "TeamTavern has relaunched. Say who you're looking for, "
            <> "and the feed shows who fits you first."
        , Paragraph $ "Your account came across, and every profile you had is a post now. "
            <> "Sign in as you did before. A post you haven't updated lately has expired: "
            <> "renew it from the home page and it's back in the feed."
        , Paragraph $ "Confirm this address and we'll email you when a new post fits yours, "
            <> "when yours is about to expire, and when someone messages you:"
        , Button { label: "Confirm email", path: "/confirm-email?nonce=" <> toString nonce }
        , Note $ "Until you do, this is the only email TeamTavern sends it. "
            <> "If you'd rather not hear from us, ignore this email."
        ]
    , unsubscribe: true
    }

send :: Mailer -> Pool -> Recipient -> Async (InternalTerror_ ()) Unit
send mailer pool { id, nickname, email } = do
    nonce <- addConfirmation pool id email
    deliver mailer $ relaunchEmail { email, nickname, nonce }

-- | One at a time, so SendGrid sees a trickle; a failure is logged and the
-- | rest go on.
sendAll :: Mailer -> Pool -> Array Recipient -> Async (InternalTerror_ ()) Int
sendAll mailer pool recipients = recipients # traverse sendOne <#> sum
    where
    sendOne recipient = do
        result <- attempt $ send mailer pool recipient
        case result of
            Left error -> fromEffect $ logError ("Error sending the relaunch email to " <> recipient.email) error $> 0
            Right _ -> pure 1

run :: Boolean -> Mailer -> Pool -> Async (InternalTerror_ ()) Unit
run sending' mailer pool = do
    recipients :: Array Recipient <- queryMany_ pool recipientsQuery
    fromEffect $ logStamped $ show (length recipients) <> " players to email."
    when sending' do
        sent <- sendAll mailer pool recipients
        fromEffect $ logStamped $ "Sent " <> show sent <> " of " <> show (length recipients) <> "."

main :: Effect Unit
main = either (\error -> log error *> exit 1) pure =<< runExceptT do
    environment <- loadEnvironment
    pool <- createPostgresPool
    setSendGridApiKey
    mailer <- lift $ loadMailer environment
    sending' <- lift sending
    lift $ runSafeAsync pure (alwaysRightWithEffect
        (\error -> logError "Error in the relaunch email" error *> exit 1)
        (\_ -> exit 0)
        (run sending' mailer pool))
