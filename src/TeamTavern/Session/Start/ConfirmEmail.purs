module TeamTavern.Session.Start.ConfirmEmail where

import Prelude

import Async (Async, left)
import Data.Bifunctor.Label (label)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, inj)
import Postgres.Async.Query (query)
import Postgres.Error (Error)
import Postgres.Query (class Querier, Query(..), QueryParameter, toQueryParameter)
import Postgres.Result (rowCount)
import TeamTavern.Player.Domain.Id (Id)
import TeamTavern.Player.Domain.Nonce (Nonce)

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

confirmEmailQuery :: Query
confirmEmailQuery = Query """
    update player
    set email_confirmed = true
    where player.id = $1 and player.confirmation_nonce = $2
    """

confirmEmailParameters :: ConfirmEmailModel -> Array QueryParameter
confirmEmailParameters { id, nonce } =
    [show $ unwrap id, unwrap nonce] <#> toQueryParameter

confirmEmail :: forall querier errors. Querier querier =>
    ConfirmEmailModel -> querier -> Async (ConfirmEmailError errors) Unit
confirmEmail model @ { id, nonce } querier = do
    result <- querier
        # query confirmEmailQuery (confirmEmailParameters model)
        # label (SProxy :: SProxy "databaseError")
    case rowCount result of
        1 -> pure unit
        _ -> left $ inj (SProxy :: SProxy "nothingConfirmed")
            { id, nonce }
