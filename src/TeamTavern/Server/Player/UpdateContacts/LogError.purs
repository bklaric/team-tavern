module TeamTavern.Server.Player.UpdateContacts.LogError where

-- import Prelude

-- import Data.Array as Array
-- import Type.Proxy (Proxy(..))
-- import Data.Variant (Variant, match)
-- import Effect (Effect, foreachE)
-- import Prim.Row (class Lacks)
-- import Record.Builder (Builder)
-- import Record.Builder as Builder
-- import TeamTavern.Server.Infrastructure.Log (clientHandler, internalHandler, logt, notAuthenticatedHandler, notAuthorizedHandler)
-- import TeamTavern.Server.Infrastructure.Log as Log
-- import TeamTavern.Server.Player.UpdateContacts.ValidateContacts (ContactsErrors)

-- type UpdateContactsError = Variant
--     ( internal :: Array String
--     , client :: Array String
--     , notAuthenticated :: Array String
--     , notAuthorized :: Array String
--     , playerContacts :: ContactsErrors
--     )

-- contactsHandler :: forall fields. Lacks "playerContacts" fields =>
--     Builder (Record fields) { playerContacts :: ContactsErrors -> Effect Unit | fields }
-- contactsHandler = Builder.insert (Proxy :: _ "playerContacts") \errors ->
--     foreachE (Array.fromFoldable errors) $ match
--     { discordTag: logt
--     , steamId: logt
--     , riotId: logt
--     , battleTag: logt
--     , eaId: logt
--     , ubisoftUsername: logt
--     , psnId: logt
--     , gamerTag: logt
--     , friendCode: logt
--     }

-- logError :: UpdateContactsError -> Effect Unit
-- logError = Log.logError "Error updating player contacts"
--     ( internalHandler
--     >>> clientHandler
--     >>> notAuthenticatedHandler
--     >>> notAuthorizedHandler
--     >>> contactsHandler
--     )
