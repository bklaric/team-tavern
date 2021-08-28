module TeamTavern.Server.Player.UpdateContacts (updateContacts) where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Newtype (unwrap)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)
import TeamTavern.Server.Player.UpdateContacts.LogError (logError)
import TeamTavern.Server.Player.UpdateContacts.SendResponse (sendResponse)
import TeamTavern.Server.Player.UpdateContacts.ValidateContacts (validateContacts)
import TeamTavern.Server.Player.UpdateContacts.WriteContacts (writeContacts)

updateContacts :: forall left.
    Pool -> String -> Cookies -> Body -> Async left Response
updateContacts pool nickname cookies body =
    sendResponse $ examineLeftWithEffect logError do
    -- Read requestor info from cookies.
    cookieInfo <- ensureSignedInAs pool cookies nickname

    -- Read contacts from body.
    contacts' <- readJsonBody body

    -- Validate contacts.
    contacts <- validateContacts contacts'

    -- Update contacts.
    writeContacts pool (unwrap cookieInfo.id) contacts
