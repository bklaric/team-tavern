module TeamTavern.Profile.View.LogError where

import Prelude

import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Console (log)
import Foreign (Foreign, MultipleErrors)
import Global.Unsafe (unsafeStringify)
import Postgres.Error (Error)
import TeamTavern.Infrastructure.Log (logt, print)
import TeamTavern.Profile.Routes (IdentifiersSingle)

type ViewError = Variant
    ( databaseError :: Error
    , unreadableDto ::
        { foreignDto :: Foreign
        , errors :: MultipleErrors
        }
    , notFound :: IdentifiersSingle
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
