module TeamTavern.Server.Session.Start.CheckProvider (checkProvider) where

import Prelude

import Async (Async)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toNullable)
import Data.Traversable (for)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Server.Infrastructure.Postgres (queryFirstMaybe)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.ValidateEmail (VouchedEmail)
import TeamTavern.Server.Player.Domain.Provider (Provider, idColumn)
import TeamTavern.Server.Player.Infrastructure.SendConfirmation (Confirmation, addConfirmation)

-- The provider's address fills a missing email and never replaces one, since
-- the player may have chosen another.
queryString :: Provider -> Query
queryString provider = Query $ """
    with found as (
        select id, nickname, email
        from player
        where """ <> idColumn provider <> """ = $1
        for update
    ),
    filled as (
        update player
        set email = $2::text, email_confirmed = $3::boolean
        from found
        where player.id = found.id
            and found.email is null
            and $2::text is not null
        returning player.id
    )
    select found.id, found.nickname, exists (select 1 from filled) as filled
    from found
    """

-- | The player who signs in with the account the provider knows by
-- | `providerId`, if any, with a missing email filled in from the address the
-- | provider vouched for. One the provider didn't verify gets the link, whose
-- | nonce is added with the rest, so it runs inside the session's transaction.
checkProvider :: ∀ querier errors. Querier querier =>
    Provider -> querier -> String -> Maybe VouchedEmail
    -> Async (InternalTerror_ errors) (Maybe { id :: Int, confirmation :: Maybe Confirmation })
checkProvider provider querier providerId email = do
    found :: Maybe { id :: Int, nickname :: String, filled :: Boolean } <-
        queryFirstMaybe querier (queryString provider)
            (providerId : toNullable (email <#> _.email) :| maybe false _.confirmed email)
    for found \{ id, nickname, filled } -> do
        confirmation <- case email of
            Just { email: email', confirmed: false } | filled -> do
                nonce <- addConfirmation querier id email'
                pure $ Just { email: email', nickname, nonce }
            _ -> pure Nothing
        pure { id, confirmation }
