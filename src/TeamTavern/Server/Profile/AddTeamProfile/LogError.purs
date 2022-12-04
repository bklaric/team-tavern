module TeamTavern.Server.Profile.AddTeamProfile.LogError where

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
-- import TeamTavern.Server.Profile.AddTeamProfile.ValidateProfile (ProfileErrors)
-- import TeamTavern.Server.Team.Infrastructure.ValidateContacts (ContactsErrors)
-- import Type.Function (type ($))
-- import Type.Proxy (Proxy(..))

-- type AddProfileError = Variant
--     ( internal :: Array String
--     , client :: Array String
--     , notAuthenticated :: Array String
--     , notAuthorized :: Array String
--     , invalidBody :: NonEmptyList $ Variant
--         ( teamProfile :: ProfileErrors
--         , teamContacts :: ContactsErrors
--         )
--     )

-- invalidBodyHandler :: forall fields. Lacks "invalidBody" fields =>
--     Builder (Record fields)
--     { invalidBody ::
--         NonEmptyList $ Variant
--         ( teamProfile :: ProfileErrors
--         , teamContacts :: ContactsErrors
--         )
--         -> Effect Unit
--     | fields }
-- invalidBodyHandler = Builder.insert (Proxy :: _ "invalidBody") \errors ->
--     foreachE (Array.fromFoldable errors) $ match
--     { teamProfile: \errors' -> logt $ "Team profile errors: " <> show errors'
--     , teamContacts: \errors' -> logt $ "Team contacts errors: " <> show errors'
--     }

-- logError :: AddProfileError -> Effect Unit
-- logError = Log.logError "Error creating team profile"
--     ( internalHandler
--     >>> clientHandler
--     >>> notAuthenticatedHandler
--     >>> notAuthorizedHandler
--     >>> invalidBodyHandler
--     )
