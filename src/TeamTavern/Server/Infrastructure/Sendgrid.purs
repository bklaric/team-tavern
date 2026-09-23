module TeamTavern.Server.Infrastructure.Sendgrid (module Sendgrid, sendAsync) where

import Prelude

import Async (Async)
import Async.Promisey (promiseToAsync)
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Jarilo (internal__)
import JavaScript.Npm.Sendgrid (Message)
import JavaScript.Npm.Sendgrid (Message, send, setApiKey, setBaseUrl) as Sendgrid
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)

sendAsync :: ∀ errors. Message -> Async (InternalTerror_ errors) Unit
sendAsync message =
    Sendgrid.send message
    # promiseToAsync
    # lmap (print >>> ("Error sending email: " <> _) >>> singleton >>> Terror internal__)
