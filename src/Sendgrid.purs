module Sendgrid where

import Prelude

import Async (Async, fromEitherCont)
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect (Effect)
import Jarilo (internal__)
import Node.Errors (Error)
import TeamTavern.Server.Infrastructure.Error (InternalError_, TavernError(..))
import TeamTavern.Server.Infrastructure.Log (print)

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

sendAsync :: forall errors. Message -> Async (InternalError_ errors) Unit
sendAsync message =
    lmap (print >>> ("Error sending email: " <> _) >>> singleton >>> TavernError internal__)
    $ fromEitherCont
    $ flip send message
