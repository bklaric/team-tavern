module Sendgrid where

import Prelude

import Async (Async, fromEitherCont)
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect (Effect)
import Jarilo (internal__)
import Node.Errors (Error)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)

type Message =
  { to :: String
  , from :: String
  , subject :: String
  , text :: String
  , html :: String
  }

foreign import setApiKey :: String -> Effect Unit

foreign import sendImpl :: (Error -> Effect Unit) -> Effect Unit -> Message -> Effect Unit

send :: (Either Error Unit -> Effect Unit) -> Message -> Effect Unit
send callback message = sendImpl (callback <<< Left) (callback $ Right unit) message

sendAsync :: ∀ errors. Message -> Async (InternalTerror_ errors) Unit
sendAsync message =
    lmap (print >>> ("Error sending email: " <> _) >>> singleton >>> Terror internal__)
    $ fromEitherCont
    $ flip send message
