module TeamTavern.Server.Infrastructure.Log where

import Prelude

import Data.Array as Array
import Data.String as String
import Effect (Effect)
import JavaScript.Error (message, name)
import JavaScript.Node.Errors.Class (class NodeError, code)
import Log (logStamped)
import TeamTavern.Server.Infrastructure.Error (Terror(..))

logError :: ∀ errors. String -> Terror errors -> Effect Unit
logError heading (Terror _ lines) =
    logStamped $ String.joinWith " | " $ Array.cons heading lines

print :: ∀ error. NodeError error => error -> String
print error =
    "Code: " <> code error
    <> "; Name: " <> name error
    <> "; Message: " <> message error
