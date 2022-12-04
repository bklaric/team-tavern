module TeamTavern.Server.Profile.AddPlayerProfile.LogError where

-- import Prelude

-- import Data.Array as Array
-- import Data.List.Types (NonEmptyList)
-- import Data.Variant (Variant, match)
-- import Effect (Effect, foreachE)
-- import Prim.Row (class Lacks)
-- import Record.Builder (Builder)
-- import Record.Builder as Builder
-- import TeamTavern.Server.Infrastructure.Log (clientHandler, internalHandler, logt, notAuthenticatedHandler, notAuthorizedHandler)
-- import TeamTavern.Server.Infrastructure.Log as Log
-- import TeamTavern.Server.Player.UpdateContacts.ValidateContacts (ContactsErrors)
-- import TeamTavern.Server.Profile.AddPlayerProfile.ValidateProfile (ProfileErrors)
-- import Type.Function (type ($))
-- import Type.Proxy (Proxy(..))

-- type CreateError = Variant
--     ( internal :: Array String
--     , client :: Array String
--     , notAuthenticated :: Array String
--     , notAuthorized :: Array String
--     , invalidBody :: NonEmptyList $ Variant
--         ( playerProfile :: ProfileErrors
--         , playerContacts :: ContactsErrors
--         )
--     )

-- invalidBodyHandler :: forall fields. Lacks "invalidBody" fields =>
--     Builder (Record fields)
--     { invalidBody ::
--         NonEmptyList $ Variant
--         ( playerProfile :: ProfileErrors
--         , playerContacts :: ContactsErrors
--         )
--         -> Effect Unit
--     | fields }
-- invalidBodyHandler = Builder.insert (Proxy :: _ "invalidBody") \errors ->
--     foreachE (Array.fromFoldable errors) $ match
--     { playerProfile: \errors' -> logt $ "Player profile errors: " <> show errors'
--     , playerContacts: \errors' -> logt $ "Player contacts errors: " <> show errors'
--     }

-- logError :: CreateError -> Effect Unit
-- logError = Log.logError "Error creating player profile"
--     ( internalHandler
--     >>> clientHandler
--     >>> notAuthenticatedHandler
--     >>> notAuthorizedHandler
--     >>> invalidBodyHandler
--     )
