module TeamTavern.Player.Register.Database (DatabaseError, addPlayer) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Variant (SProxy(SProxy), Variant, inj)
import Node.Errors.Class (code)
import Postgres.Error (Error, constraint)
import Postgres.Error.Codes (unique_violation)
import Postgres.Query (class Querier, Query(..), QueryParameter(..))
import TeamTavern.Architecture.Async (label)
import TeamTavern.Architecture.Postgres.Query (query)
import TeamTavern.Player.Email (Email)
import TeamTavern.Player.Nickname (Nickname)
import TeamTavern.Player.Register.PlayerToRegister (PlayerToRegister)

insertPlayerQuery :: Query
insertPlayerQuery =
    Query "insert into player (email, nickname, token) values ($1, $2, $3)"

insertPlayerParameters :: PlayerToRegister -> Array QueryParameter
insertPlayerParameters { email, nickname, token } =
    [unwrap email, unwrap nickname, unwrap token] <#> QueryParameter

type DatabaseError = Variant
    ( emailTaken :: { email :: Email, error :: Error }
    , nicknameTaken :: { nickname :: Nickname, error :: Error }
    , other :: { error :: Error }
    )

addPlayer
    :: forall errors querier
    .  Querier querier
    => querier
    -> PlayerToRegister
    -> Async (Variant (database :: DatabaseError | errors)) PlayerToRegister
addPlayer querier playerToRegister =
    query insertPlayerQuery (insertPlayerParameters playerToRegister) querier
    # lmap (\error ->
        case code error == unique_violation of
        true | constraint error == Just "player_email_key" -> inj
            (SProxy :: SProxy "emailTaken")
            { email: playerToRegister.email, error }
        true | constraint error == Just "player_nickname_key" -> inj
            (SProxy :: SProxy "nicknameTaken")
            { nickname: playerToRegister.nickname, error }
        _ -> inj (SProxy :: SProxy "other") { error })
    # label (SProxy :: SProxy "database")
    # map (const playerToRegister)
