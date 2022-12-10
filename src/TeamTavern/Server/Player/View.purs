module TeamTavern.Server.Player.View where

import Prelude

import Data.Variant (Variant)
import Jarilo (InternalRow_, NotFoundRow_, OkRow, ok_)
import Tasync (Tasync)
import TeamTavern.Server.Infrastructure.CheckSignedIn (checkSignedIn')
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse')
import TeamTavern.Server.Player.View.LoadPlayer (loadPlayer)
import Type.Row (type (+))

view ∷ ∀ l. Tasync (nickname ∷ String) (timezone ∷ String) Unit l (Variant (InternalRow_ + NotFoundRow_ + OkRow _ ()))
view = sendResponse' do
    -- Check if player is signed in.
    cookieInfo <- checkSignedIn'

    -- Load player.
    ok_ <$> loadPlayer cookieInfo
