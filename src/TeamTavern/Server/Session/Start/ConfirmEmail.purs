module TeamTavern.Server.Session.Start.ConfirmEmail where

import Prelude

import Async (Async, left)
import Data.Bifunctor.Label (label)
import Data.Variant (SProxy(..), Variant, inj)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Query (class Querier, Query(..), QueryParameter, (:|))
import Postgres.Result (rowCount)
import TeamTavern.Server.Player.Domain.Id (Id)
import TeamTavern.Server.Player.Domain.Nonce (Nonce)

type ConfirmEmailModel =
    { id :: Id
    , nonce :: Nonce
    }

type ConfirmEmailError errors = Variant
    ( databaseError :: Error
    , nothingConfirmed ::
        { id :: Id
        , nonce :: Nonce
        }
    | errors )

queryString :: Query
queryString = Query """
    update player
    set email_confirmed = true
    where player.id = $1 and player.confirmation_nonce = $2
    """

queryParameters :: ConfirmEmailModel -> Array QueryParameter
queryParameters { id, nonce } = id :| nonce

confirmEmail :: forall querier errors. Querier querier =>
    ConfirmEmailModel -> querier -> Async (ConfirmEmailError errors) Unit
confirmEmail model @ { id, nonce } querier = do
    result <- querier
        # query queryString (queryParameters model)
        # label (SProxy :: SProxy "databaseError")
    case rowCount result of
        1 -> pure unit
        _ -> left $ inj (SProxy :: SProxy "nothingConfirmed")
            { id, nonce }
