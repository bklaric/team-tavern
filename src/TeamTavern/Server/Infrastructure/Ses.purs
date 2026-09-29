module TeamTavern.Server.Infrastructure.Ses
    (module SesV2, createClient, sendAsync) where

import Prelude

import Async (Async)
import Async.Promisey (promiseToAsync)
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Effect (Effect)
import Jarilo (internal__)
import JavaScript.Error (message, name)
import JavaScript.Npm.AwsSdk.SesV2 (Credentials, SendEmailParams, SesV2Client, newClient, sendEmail)
import JavaScript.Npm.AwsSdk.SesV2 (Credentials, SendEmailParams, SesV2Client) as SesV2
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)

-- | A client of the region teamtavern.net is verified in.
createClient :: Credentials -> Effect SesV2Client
createClient credentials = newClient { credentials, region: "eu-central-1" }

sendAsync :: ∀ errors. SesV2Client -> SendEmailParams -> Async (InternalTerror_ errors) Unit
sendAsync client params =
    sendEmail params client
    # promiseToAsync
    # lmap (\error -> Terror internal__ $ singleton $
        "Error sending email: " <> name error <> ": " <> message error)
