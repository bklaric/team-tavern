module TeamTavern.Server.Player.UpdateContacts (updateContacts) where

import Prelude

import Async (Async, examineLeftWithEffect)
import Data.Newtype (unwrap)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import TeamTavern.Routes.Player.UpdatePlayerContacts as UpdatePlayerContacts
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Infrastructure.Postgres (transaction)
import TeamTavern.Server.Infrastructure.ReadJsonBody (readJsonBody)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Player.UpdateContacts.LoadRequiredPlatforms (loadRequiredPlatforms)
import TeamTavern.Server.Player.UpdateContacts.ValidateContacts (validateContacts)
import TeamTavern.Server.Player.UpdateContacts.WriteContacts (writeContacts)

updateContacts :: forall left.
    Pool -> String -> Cookies -> UpdatePlayerContacts.RequestContent -> Async left _
updateContacts pool nickname cookies contacts' =
    sendResponse "Error updating player contacts" do
    -- Read requestor info from cookies.
    cookieInfo <- ensureSignedInAs pool cookies nickname

    pool # transaction \client -> do
        -- Read required platforms.
        requiredPlatforms <- loadRequiredPlatforms client (unwrap cookieInfo.id)

        -- Validate contacts.
        contacts <- validateContacts requiredPlatforms contacts'

        -- Update contacts.
        writeContacts client (unwrap cookieInfo.id) contacts
