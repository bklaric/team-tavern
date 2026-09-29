module TeamTavern.Server.Worker (startWorker) where

import Prelude

import Async (Async, alwaysRightWithEffect, runSafeAsync)
import Effect (Effect)
import Effect.Ref as Ref
import JavaScript.Date (Date)
import JavaScript.Date as Date
import JavaScript.Node.Timers (setInterval)
import JavaScript.Npm.Pg.Pool (Pool)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.Log (logError)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Worker.AddExpiries (addExpiries)
import TeamTavern.Server.Worker.SendPeriodEmails (sendPeriodEmails)

-- | Every `period` seconds, marks the posts that have entered their last week
-- | and emails the period's notifications (brief 8). A run covers the time
-- | from when the one before it began, the first from when the process
-- | started, so while the process runs the periods leave no gap and, even when
-- | runs overlap, cover nothing twice. A failed run is logged, and the next
-- | runs as usual.
startWorker :: Int -> Mailer -> Pool -> Effect Unit
startWorker period mailer pool = do
    started <- Date.now
    last <- Ref.new started
    void $ setInterval (period * 1000) do
        now <- Date.now
        since <- Ref.modify' (\since -> { state: now, value: since }) last
        runSafeAsync pure
            (alwaysRightWithEffect (logError "Error in the period worker") pure (run mailer pool since now))

run :: Mailer -> Pool -> Date -> Date -> Async (InternalTerror_ ()) Unit
run mailer pool since now = do
    addExpiries pool now
    sendPeriodEmails mailer pool since now
