module TeamTavern.Server.Block.Infrastructure.FileReport (createReportLimit, fileReport) where

import Prelude

import Async (Async, attempt, fromEffect, fromEither)
import Data.Either (either)
import Effect (Effect)
import Jarilo (TooManyRequestsRow)
import JavaScript.Npm.Pg.Client (Client)
import JavaScript.Npm.Pg.Pool (Pool)
import TeamTavern.Routes.Shared.Report (ReportOutcome)
import TeamTavern.Routes.Shared.Wait (Wait)
import TeamTavern.Server.Block.Infrastructure.AddReport (Reported, ValidReport, addReport)
import TeamTavern.Server.Block.Infrastructure.SendReportEmail (AdminEmail, sendReportEmail)
import TeamTavern.Server.Infrastructure.Email (Mailer)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.RateLimit (Limiter, createLimiter, ensureAdmitted, release)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import Type.Row (type (+))

-- | Each report is mailed to the admin, so a player sends 10 a day, from posts
-- | and conversations together. One already sent, or about nothing to be
-- | found, doesn't count.
createReportLimit :: Effect Limiter
createReportLimit = createLimiter "Reports sent by one account" { count: 10, seconds: 86400 }

-- | Stores `report` from `reporter` about what `find` finds, in one
-- | transaction, and mails it to the admin, unless the reporter reported the
-- | player about the post before. A report is counted before it's stored, so
-- | reports sent at once can't all find room, and given back if none was.
fileReport :: ∀ errors.
    Limiter -> Mailer -> AdminEmail -> Pool -> Int -> ValidReport
    -> (Client -> Async (InternalTerror_ (TooManyRequestsRow Wait + errors)) Reported)
    -> Async (InternalTerror_ (TooManyRequestsRow Wait + errors)) ReportOutcome
fileReport limit mailer adminEmail pool reporter report find = do
    let account = show reporter
    ensureAdmitted limit account "Too many reports sent by one account."
    result <- attempt $ pool # transaction \client -> do
        reported <- find client
        stored <- addReport client reporter reported report
        pure { reported, stored }
    unless (either (const false) _.stored result) $ fromEffect $ release limit account
    { reported, stored } <- fromEither result
    when stored $ sendReportEmail mailer adminEmail reported report
    pure { alreadyReported: not stored }
