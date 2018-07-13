module TeamTavern.Player.Register.AddPlayer where

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
import TeamTavern.Player.Credentials (Credentials)
import TeamTavern.Player.Email (Email)
import TeamTavern.Player.Nickname (Nickname)
import TeamTavern.Player.Token (Token)

insertPlayerQuery :: Query
insertPlayerQuery =
    Query "insert into player (email, nickname) values ($1, $2) returning id"

insertTokenQuery :: Query
insertTokenQuery = Query "insert into token (player_id, value) values ($1, $2)"

insertPlayerParameters :: Email -> Nickname -> Array QueryParameter
insertPlayerParameters email nickname =
    [unwrap email, unwrap nickname] <#> QueryParameter

insertTokenParameters :: Int -> Token -> Array QueryParameter
insertTokenParameters playerId token =
    [show playerId, unwrap token] <#> QueryParameter

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

readPlayerId :: Result -> Credentials -> Async AddPlayerError Int
readPlayerId result credentials =
    readScalar result
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
    -> Async (Variant (addPlayer :: AddPlayerError | errors)) Unit
addPlayer pool credentials@{ email, nickname, token } =
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
        playerId <- readPlayerId result credentials
        query insertTokenQuery (insertTokenParameters playerId token) client
            # lmap (wrapError credentials))
            # void
    # label (SProxy :: SProxy "addPlayer")
