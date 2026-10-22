module TeamTavern.Server.ClientError.ReportClientError (ClientErrorLimits, createClientErrorLimits, reportClientError) where

import Prelude

import Async (Async, fromEffect)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.String as String
import Data.String.Regex (Regex, replace)
import Data.String.Regex.Flags (global, unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Variant (Variant)
import Effect (Effect)
import Jarilo (NoContentRow_, noContent_)
import TeamTavern.Routes.ClientError.ReportClientError as ReportClientError
import TeamTavern.Server.Infrastructure.Log (logStamped)
import TeamTavern.Server.Infrastructure.RateLimit (Admission(..), ClientLimits, Limiter, admit, clientAddress, createClientLimiter, createLimiter)
import Type.Row (type (+))

fieldLength :: Int
fieldLength = 300

-- Anyone writes a report, so each goes to the log as one line of plain text:
-- a line break would start a line that reads as the server's own, an escape
-- would move the cursor of a terminal reading the log, and a direction
-- override would show the rest of the line reversed.
controls :: Regex
controls = unsafeRegex """[\p{Cc}\p{Cf}\p{Zl}\p{Zp}]""" (global <> unicode)

-- | Anyone can send a report, so at most 10 a minute from one client and 60 a
-- | minute in all reach the log. One client can't spend everyone's budget.
type ClientErrorLimits = { perClient :: Limiter, inAll :: Limiter }

createClientErrorLimits :: ClientLimits -> Effect ClientErrorLimits
createClientErrorLimits clientLimits = do
    perClient <- createClientLimiter clientLimits "Client errors from one client" { count: 10, seconds: 60 }
    inAll <- createLimiter "Client errors" { count: 60, seconds: 60 }
    pure { perClient, inAll }

-- A report the client's limit refuses doesn't count against everyone's.
admitReport :: ClientErrorLimits -> Map String String -> Effect Boolean
admitReport { perClient, inAll } headers =
    admit perClient (clientAddress headers) >>= case _ of
        Admitted -> admit inAll "" <#> case _ of
            Admitted -> true
            Refused _ -> false
        Refused _ -> pure false

reportClientError :: ∀ left.
    ClientErrorLimits -> Map String String -> ReportClientError.RequestContent
    -> Async left (Variant (NoContentRow_ + ()))
reportClientError limits headers { page, request, failure, bundle } = do
    fromEffect do
        admitted <- admitReport limits headers
        when admitted $ logStamped $ String.joinWith " | " $ map (String.take fieldLength >>> replace controls " ")
            [ "Client error"
            , "Page: " <> page
            , "Request: " <> maybe "-" identity request
            , "Failure: " <> failure
            , "Bundle: " <> bundle
            , "User agent: " <> fromMaybe "-" (Map.lookup "user-agent" headers)
            ]
    pure noContent_
