module TeamTavern.Server.Infrastructure.Log where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Error.Class (message, name)
import Node.Errors.Class (class NodeError, code)

logt :: String -> Effect Unit
logt string = log $ "\t" <> string

print :: forall error. NodeError error => error -> String
print error =
    "Code: " <> code error
    <> "; Name: " <> name error
    <> "; Message: " <> message error
