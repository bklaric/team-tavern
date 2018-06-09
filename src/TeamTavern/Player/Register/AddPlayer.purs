module TeamTavern.Player.Register.AddPlayer where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (SProxy(..), Variant, inj)
import Node.Errors.Class (code)
import Postgres.Error (Error, constraint)
import Postgres.Error.Codes (unique_violation)
import Postgres.Pool (Pool)
import Postgres.Query (Query(..), QueryParameter(..))
import TeamTavern.Architecture.Async (label)
import TeamTavern.Architecture.Postgres.Query (query)
import TeamTavern.Player.Credentials (Credentials)

insertPlayerQuery :: Query
insertPlayerQuery =
    Query "insert into player (email, nickname, token) values ($1, $2, $3)"

insertPlayerParameters :: Credentials -> Array QueryParameter
insertPlayerParameters { email, nickname, token } =
    [unwrap email, unwrap nickname, unwrap token] <#> QueryParameter

type DatabaseError = Variant
    ( emailTaken :: { error :: Error, credentials :: Credentials }
    , nicknameTaken :: { error :: Error, credentials :: Credentials }
    , other :: { error :: Error, credentials :: Credentials }
    )

addPlayer
    :: forall errors
    .  Pool
    -> Credentials
    -> Async (Variant (database :: DatabaseError | errors)) Unit
addPlayer pool credentials =
    query insertPlayerQuery (insertPlayerParameters credentials) pool
    # lmap (\error ->
        case code error == unique_violation of
        true | constraint error == Just "player_email_key" -> inj
            (SProxy :: SProxy "emailTaken") { error, credentials }
        true | constraint error == Just "player_nickname_key" -> inj
            (SProxy :: SProxy "nicknameTaken") { error, credentials }
        _ -> inj (SProxy :: SProxy "other") { error, credentials })
    # label (SProxy :: SProxy "database")
    # void
