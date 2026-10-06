module TeamTavern.Server.Player.Register.AddPlayer (AddPlayerError, addPlayer) where

import Prelude

import Async (Async)
import Data.Tuple (Tuple(..))
import Data.Variant (inj)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Server.Infrastructure.ValidateEmail (Email)
import TeamTavern.Server.Player.Domain.Hash (Hash)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)
import TeamTavern.Server.Player.Infrastructure.InsertPlayer (InsertPlayerError, insertPlayer)
import Type.Proxy (Proxy(..))

type AddPlayerModel =
    { email :: Email
    , nickname :: Nickname
    , hash :: Hash
    }

type AddPlayerError errors errors' = InsertPlayerError (emailTaken :: {} | errors') errors

queryString :: Query
queryString = Query """
    insert into player (email, nickname, password_hash)
    values ($1, $2, $3)
    returning id
    """

addPlayer :: ∀ querier errors errors'. Querier querier =>
    querier -> AddPlayerModel -> Async (AddPlayerError errors errors') Int
addPlayer querier { email, nickname, hash } =
    insertPlayer [ Tuple "player_lower_email_key" $ inj (Proxy :: _ "emailTaken") {} ]
        querier queryString (email : nickname :| hash)
