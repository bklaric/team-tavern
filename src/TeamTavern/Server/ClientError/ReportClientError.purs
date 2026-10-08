module TeamTavern.Server.ClientError.ReportClientError (ClientErrorLimit, createClientErrorLimit, reportClientError) where

import Prelude

import Async (Async, fromEffect)
import Data.DateTime.Instant (Instant, diff)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.String as String
import Data.Time.Duration (Milliseconds(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Jarilo (NoContentRow_, noContent_)
import TeamTavern.Routes.ClientError.ReportClientError as ReportClientError
import TeamTavern.Server.Infrastructure.Log (logStamped)
import Type.Row (type (+))

-- | Anyone can send a report, so at most this many a minute reach the log, and
-- | the rest are counted.
perMinute :: Int
perMinute = 60

fieldLength :: Int
fieldLength = 300

type Minute = { start :: Instant, logged :: Int, dropped :: Int }

newtype ClientErrorLimit = ClientErrorLimit (Ref Minute)

createClientErrorLimit :: Effect ClientErrorLimit
createClientErrorLimit = do
    start <- now
    Ref.new { start, logged: 0, dropped: 0 } <#> ClientErrorLimit

-- Whether a report may be logged now. The count of those dropped is logged
-- by the first report of the next minute, as nothing runs in between.
admit :: ClientErrorLimit -> Effect Boolean
admit (ClientErrorLimit ref) = do
    time <- now
    minute <- Ref.read ref
    if diff time minute.start >= Milliseconds 60000.0
    then do
        when (minute.dropped > 0) $ logStamped $
            "Client errors dropped | " <> show minute.dropped <> " past " <> show perMinute <> " in a minute"
        Ref.write { start: time, logged: 1, dropped: 0 } ref
        pure true
    else if minute.logged < perMinute
    then Ref.write minute { logged = minute.logged + 1 } ref $> true
    else Ref.write minute { dropped = minute.dropped + 1 } ref $> false

reportClientError :: ∀ left.
    ClientErrorLimit -> Map String String -> ReportClientError.RequestContent
    -> Async left (Variant (NoContentRow_ + ()))
reportClientError limit headers { page, request, failure, bundle } = do
    fromEffect do
        admitted <- admit limit
        when admitted $ logStamped $ String.joinWith " | " $ map (String.take fieldLength)
            [ "Client error"
            , "Page: " <> page
            , "Request: " <> maybe "-" identity request
            , "Failure: " <> failure
            , "Bundle: " <> bundle
            , "User agent: " <> fromMaybe "-" (Map.lookup "user-agent" headers)
            ]
    pure noContent_
