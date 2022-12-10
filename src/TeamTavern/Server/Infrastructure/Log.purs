module TeamTavern.Server.Infrastructure.Log where

import Prelude

import Data.Formatter.DateTime (FormatterCommand(..), format)
import Data.List (List(..), (:))
import Data.String (joinWith)
import Effect (Effect, foreachE)
import Effect.Console (log)
import Effect.Now (nowDateTime)
import Error.Class (message, name)
import Node.Errors.Class (class NodeError, code)
import TeamTavern.Server.Infrastructure.Error (Terror(..))

datetimeFormat ∷ List FormatterCommand
datetimeFormat =
    YearFull : Placeholder "-" :
    MonthTwoDigits : Placeholder "-" :
    DayOfMonthTwoDigits : Placeholder " " :
    Hours24 : Placeholder ":" :
    MinutesTwoDigits : Placeholder ":" :
    SecondsTwoDigits : Nil

logStamped ∷ String -> Effect Unit
logStamped string =
    nowDateTime
    <#> format datetimeFormat
    >>= \dateTime -> log $ dateTime <> " - " <> string

logt ∷ String -> Effect Unit
logt string = log $ "    " <> string

logLines ∷ Array String -> Effect Unit
logLines lines = foreachE lines logt

logError ∷ ∀ error. String -> Terror error -> Effect Unit
logError heading (Terror _ lines) = do
    logStamped heading
    logLines lines

-- logError request (Terror response lines) = do
--     logStamped $ joinWith " " [request.endpoint, show request.path, show request.query, show request.body, show response]
--     logLines lines

print ∷ ∀ error. NodeError error => error -> String
print error =
    "Code: " <> code error
    <> "; Name: " <> name error
    <> "; Message: " <> message error
