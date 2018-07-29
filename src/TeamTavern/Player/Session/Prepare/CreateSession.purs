module TeamTavern.Player.Session.Prepare.CreateSession
    (CreateSessionError, createSession) where

import Prelude

import Async (Async, fromEither)
import Data.Bifunctor (lmap)
import Data.Either (note)
import Data.List.Types (NonEmptyList)
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant)
import Postgres.Error (Error)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import Postgres.Result (Result, readScalar)
import TeamTavern.Architecture.Async (fromValidated, label)
import TeamTavern.Architecture.Postgres.Query (query)
import TeamTavern.Player.Domain.Email (Email, EmailError)
import TeamTavern.Player.Domain.Email as Email
import TeamTavern.Player.Domain.Types (NicknamedSecrets)

createSessionQuery :: Query
createSessionQuery = Query """
    with insert_result (player_id) as
    (   insert into session (player_id, token, nonce)
        select player.id, $2, $3
        from player
        where player.nickname = $1
        returning player_id
    )
    select player.email
    from insert_result
        join player on player.id = insert_result.player_id
    """

createSessionParameters :: NicknamedSecrets -> Array QueryParameter
createSessionParameters { nickname, token, nonce } =
    [unwrap nickname, unwrap token, unwrap nonce] <#> QueryParameter

type CreateSessionError =
    { secrets :: NicknamedSecrets
    , error :: Variant
        ( unknownNickname :: Result
        , invalidEmail :: NonEmptyList EmailError
        , other :: Error
        )
    }

createSession
    :: forall errors
    .  Pool
    -> NicknamedSecrets
    -> Async (Variant (createSession :: CreateSessionError | errors)) Email
createSession pool secrets = label (SProxy :: SProxy "createSession") do
    result <- pool
        # query createSessionQuery (createSessionParameters secrets)
        # label (SProxy :: SProxy "other")
        # lmap { error: _, secrets }
    email <- readScalar result
        # note result
        # fromEither
        # label (SProxy :: SProxy "unknownNickname")
        # lmap { error: _, secrets }
    Email.create email
        # fromValidated
        # label (SProxy :: SProxy "invalidEmail")
        # lmap { error: _, secrets }
