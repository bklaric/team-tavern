module TeamTavern.Server.Player.Register.AddPlayer (AddPlayerError, addPlayer) where

import Prelude

import Async (Async)
import Data.Variant (inj)
import Jarilo (InternalRow_, BadRequestRow, badRequest_)
import Postgres.Query (class Querier, Query(..), (:|))
import TeamTavern.Routes.Player.RegisterPlayer as RegisterPlayer
import TeamTavern.Server.Infrastructure.Error (TerrorVar)
import TeamTavern.Server.Infrastructure.Postgres (queryFirst)
import TeamTavern.Server.Player.Domain.Hash (Hash)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

type AddPlayerModel =
    { nickname :: Nickname
    , hash :: Hash
    }

type AddPlayerError errors = TerrorVar
    ( InternalRow_
    + BadRequestRow RegisterPlayer.BadContent
    + errors )

queryString :: Query
queryString = Query """
    insert into player (nickname, password_hash)
    values ($1, $2)
    returning id
    """

addPlayer :: forall querier errors. Querier querier =>
    querier -> AddPlayerModel -> Async (AddPlayerError errors) Int
addPlayer pool { nickname, hash } = do
    let nicknameTakenResponse = badRequest_ $ inj (Proxy :: _ "nicknameTaken") {}
    { id } :: { id :: Int } <- queryFirst nicknameTakenResponse pool queryString (nickname :| hash)
    pure id
