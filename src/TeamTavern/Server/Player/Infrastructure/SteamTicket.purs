module TeamTavern.Server.Player.Infrastructure.SteamTicket (addSteamTicket, takeSteamTicket) where

import Prelude

import Async (Async, note)
import Data.Newtype (unwrap)
import Data.Variant (Variant, inj)
import Jarilo (BadRequestRow, InternalRow_, badRequest_)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstMaybe, queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Session.Domain.Token as Token
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

-- Each new ticket clears out those past their hour.
addQuery :: Query
addQuery = Query """
    with stale as (
        delete from steam_ticket where created < now() - interval '1 hour'
    )
    insert into steam_ticket (token_hash, steam_id)
    values ($1, $2)
    """

takeQuery :: Query
takeQuery = Query """
    delete from steam_ticket
    where token_hash = $1 and created > now() - interval '1 hour'
    returning steam_id as "steamId"
    """

-- | A ticket for registering the Steam account Steam has vouched for, which
-- | the player shows once they have picked a nickname.
addSteamTicket :: ∀ querier errors. Querier querier =>
    querier -> String -> Async (InternalTerror_ errors) String
addSteamTicket querier steamId = do
    token <- Token.generate
    tokenHash <- Token.hash token
    queryNone querier addQuery (tokenHash :| steamId)
    pure $ unwrap token

-- | The Steam account the ticket is for, which the ticket is then spent on.
-- | One past its hour, or spent, is `steamTicket`.
takeSteamTicket :: ∀ querier other errors. Querier querier =>
    querier -> String
    -> Async (TerrorVar (InternalRow_ + BadRequestRow (Variant (steamTicket :: {} | other)) + errors)) String
takeSteamTicket querier ticket = do
    tokenHash <- Token.hash (Token.Token ticket)
    row :: _ { steamId :: String } <- queryFirstMaybe querier takeQuery (tokenHash : [])
    row <#> _.steamId # note (Terror (badRequest_ $ inj (Proxy :: _ "steamTicket") {})
        [ "The Steam ticket is spent or past its hour." ])
