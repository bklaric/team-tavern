module Sendgrid where

import Prelude

import Async (Async, fromEitherCont)
import Data.Array (singleton)
import Data.Bifunctor.Label (labelMap)
import Data.Either (Either(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Node.Errors (Error)
import TeamTavern.Server.Infrastructure.Error (InternalError)
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

sendAsync :: forall errors. Message -> Async (InternalError errors) Unit
sendAsync message =
    labelMap (SProxy :: _ "internal") (singleton <<< print) $ fromEitherCont $ flip send message
