module Postmark.Async.Client where

import Prelude

import Async (Async, fromEitherCont)
import Postmark.Client (Client)
import Postmark.Client as Postmark
import Postmark.Error (Error)
import Postmark.Message (Message)

sendEmail :: Message -> Client -> Async Error Unit
sendEmail message client = void $ fromEitherCont \callback ->
    Postmark.sendEmail message callback client
