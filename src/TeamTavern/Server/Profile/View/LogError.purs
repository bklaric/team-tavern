module TeamTavern.Server.Profile.View.LogError where

import Prelude

import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Console (log)
import Foreign (Foreign, MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import TeamTavern.Server.Infrastructure.Log (logt, print)
import TeamTavern.Server.Profile.Routes (Identifiers)

type ViewError = Variant
    ( databaseError :: Error
    , unreadableDto ::
        { foreignDto :: Foreign
        , errors :: MultipleErrors
        }
    , notFound :: Identifiers
    )

logError :: ViewError -> Effect Unit
logError viewError = do
    log "Error viewing profile"
    viewError # match
        { databaseError: \error ->
            logt $ "Unknown database error ocurred: " <> print error
        , unreadableDto: \{ foreignDto, errors } -> do
            logt $ "Couldn't read database dto: "
                <> unsafeStringify foreignDto
            logt $ "Reading resulted in these errors: " <> show errors
        , notFound: \identifiers ->
            logt $ "Profile wasn't found: " <> show identifiers
        }