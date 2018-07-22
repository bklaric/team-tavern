module TeamTavern.Player.Register.AddPlayer (AddPlayerError, addPlayer) where

import Prelude

import Async (Async, fromEither)
import Data.Bifunctor (lmap)
import Data.Either (note)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, inj)
import Node.Errors.Class (code)
import Postgres.Error (Error, constraint)
import Postgres.Error.Codes (unique_violation)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (Result, readScalar)
import TeamTavern.Architecture.Async (label)
import TeamTavern.Architecture.Either as Either
import TeamTavern.Architecture.Postgres.Pool (withTransaction)
import TeamTavern.Architecture.Postgres.Query (query)
import TeamTavern.Player.Register.Types.Credentials (Credentials, IdentifiedCredentials)
import TeamTavern.Player.Domain.Email (Email)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Nonce (Nonce)
import TeamTavern.Player.Domain.PlayerId (PlayerId)
import TeamTavern.Player.Domain.PlayerId as PlayerId
import TeamTavern.Player.Domain.Token (Token)

insertPlayerQuery :: Query
insertPlayerQuery =
    Query "insert into player (email, nickname) values ($1, $2) returning id"

insertTokenQuery :: Query
insertTokenQuery =
    Query "insert into token (player_id, value, nonce) values ($1, $2, $3)"

insertPlayerParameters :: Email -> Nickname -> Array QueryParameter
insertPlayerParameters email nickname =
    [unwrap email, unwrap nickname] <#> QueryParameter

insertTokenParameters :: PlayerId -> Token -> Nonce -> Array QueryParameter
insertTokenParameters playerId token nonce =
    [show playerId, unwrap token, unwrap nonce] <#> QueryParameter

type AddPlayerError = Variant
    ( emailTaken ::
        { error :: Error, credentials :: Credentials }
    , nicknameTaken ::
        { error :: Error, credentials :: Credentials }
    , cantReadPlayerId ::
        { result :: Result, credentials :: Credentials }
    , other ::
        { error :: Error, credentials :: Credentials }
    )

_emailTaken = SProxy :: SProxy "emailTaken"

_nicknameTaken = SProxy :: SProxy "nicknameTaken"

_noSingleInsertResult = SProxy :: SProxy "noSingleInsertResult"

_cantReadPlayerId = SProxy :: SProxy "cantReadPlayerId"

_other = SProxy :: SProxy "other"

readPlayerId :: Result -> Credentials -> Async AddPlayerError PlayerId
readPlayerId result credentials =
    readScalar result
    >>= PlayerId.create
    # note { result, credentials }
    # Either.label _cantReadPlayerId
    # fromEither

wrapError
    :: forall errors
    .  Credentials
    -> Error
    -> Variant
        (other :: { error :: Error, credentials :: Credentials } | errors)
wrapError credentials = { error: _, credentials } >>> inj _other

addPlayer
    :: forall errors
    .  Pool
    -> Credentials
    -> Async
        (Variant (addPlayer :: AddPlayerError | errors))
        IdentifiedCredentials
addPlayer pool credentials@{ email, nickname, token, nonce } =
    pool # withTransaction (wrapError credentials) (\client -> do
        result <-
            query
                insertPlayerQuery (insertPlayerParameters email nickname) client
                # lmap (\error ->
                    case code error == unique_violation of
                    true | constraint error == Just "player_email_key" ->
                        inj _emailTaken { error, credentials }
                    true | constraint error == Just "player_nickname_key" ->
                        inj _nicknameTaken { error, credentials }
                    _ -> inj _other { error, credentials })
        id <- readPlayerId result credentials
        query insertTokenQuery (insertTokenParameters id token nonce) client
            # lmap (wrapError credentials)
            # void
        pure { id, email, nickname, token, nonce })
    # label (SProxy :: SProxy "addPlayer")
