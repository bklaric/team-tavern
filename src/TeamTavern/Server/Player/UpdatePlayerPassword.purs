module TeamTavern.Server.Player.UpdatePlayerPassword where

import Prelude

import Async (Async)
import Jarilo (noContent_)
import Postgres.Pool (Pool)
import Postgres.Query (class Querier, Query(..), (:|))
import TeamTavern.Routes.Player.UpdatePlayerPassword as UpdatePlayerPassword
import TeamTavern.Server.Infrastructure.Cookie (Cookies)
import TeamTavern.Server.Infrastructure.EnsureSignedInAs (ensureSignedInAs)
import TeamTavern.Server.Infrastructure.Postgres (queryNone, transaction)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import TeamTavern.Server.Player.Domain.Hash (Hash, generateHash)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Player.Domain.Password (validatePassword')
import TeamTavern.Server.Player.UpdatePlayerEmail (checkPassword)

emailQueryString :: Query
emailQueryString = Query """
    update player
    set password_hash = $2
    where id = $1
    """

updatePassword :: forall errors querier. Querier querier =>
    Id -> Hash -> querier -> Async (InternalTerror_ errors) Unit
updatePassword id hash client =
    queryNone client emailQueryString (id :| hash)

updatePlayerPassword :: ∀ left.
    Pool -> String -> Cookies -> UpdatePlayerPassword.RequestContent -> Async left _
updatePlayerPassword pool nickname cookies {passwordOld, passwordNew} =
    sendResponse "Error updating player email" do
    -- Read requestor info from cookies.
    {id} <- ensureSignedInAs pool cookies nickname

    -- Validate new password.
    passwordNewValid <- validatePassword' passwordNew

    -- Generate new password hash.
    hash <- generateHash passwordNewValid

    pool # transaction \client -> do
        -- Make sure the current password is correct.
        checkPassword nickname passwordOld client

        -- Update password.
        updatePassword id hash client

    pure $ noContent_
