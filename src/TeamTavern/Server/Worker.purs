module TeamTavern.Server.Worker (startWorker) where

import Prelude

import Async (Async, alwaysRightWithEffect, runSafeAsync)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Ref as Ref
import JavaScript.Date (Date, getTime)
import JavaScript.Date as Date
import JavaScript.Node.Timers (setInterval)
import JavaScript.Npm.Pg.Pool (Pool)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.Log (logError)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Worker.AddExpiries (addExpiries)
import TeamTavern.Server.Worker.PruneSessions (pruneSessions)
import TeamTavern.Server.Worker.SendPeriodEmails (sendPeriodEmails)

-- | Milliseconds after which a run still going is taken for stuck on a call
-- | that never answers, and the next runs anyway.
stuckAfter :: Number
stuckAfter = 15.0 * 60.0 * 1000.0

-- | Every `period` seconds, marks the posts that have entered their last week,
-- | emails the notifications not emailed yet (brief 8) and deletes the
-- | sessions that can no longer sign anyone in, each step logged on its own if
-- | it fails. What a run leaves undone, failed or cut short by a restart, the
-- | next does. A run due while the one before it is still going is skipped, so
-- | two don't email the same notification.
startWorker :: Int -> Mailer -> Pool -> Effect Unit
startWorker period mailer pool = do
    going <- Ref.new (Nothing :: Maybe Date)
    void $ setInterval (period * 1000) do
        now <- Date.now
        began <- Ref.read going
        unless (began # maybe false \began' -> getTime now - getTime began' < stuckAfter) do
            Ref.write (Just now) going
            runSafeAsync
                (\_ -> Ref.modify_ (\began' -> if (getTime <$> began') == Just (getTime now) then Nothing else began') going)
                (run mailer pool now)

run :: ∀ left. Mailer -> Pool -> Date -> Async left Unit
run mailer pool now = do
    step "Error adding expiry notifications" $ addExpiries pool now
    step "Error sending period emails" $ sendPeriodEmails mailer pool now
    step "Error pruning sessions" $ pruneSessions pool
    where
    step :: String -> Async (InternalTerror_ ()) Unit -> Async left Unit
    step heading = alwaysRightWithEffect (logError heading) pure
