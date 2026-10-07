module TeamTavern.Server.Player.Infrastructure.SignInTicket (Ticketed, addSignInTicket, takeSignInTicket) where

import Prelude

import Async (Async, note)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import Jarilo (BadRequestRow, InternalRow_, badRequest_)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar)
import TeamTavern.Server.Infrastructure.Postgres (queryFirstMaybe, queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.ValidateEmail (VouchedEmail)
import TeamTavern.Server.Player.Domain.Provider (Provider, ticketName)
import TeamTavern.Server.Session.Domain.Token as Token
import Type.Row (type (+))

-- Each new ticket clears out those past their hour.
addQuery :: Query
addQuery = Query """
    with stale as (
        delete from sign_in_ticket where created < now() - interval '1 hour'
    )
    insert into sign_in_ticket (token_hash, provider, provider_id, email, email_confirmed)
    values ($1, $2, $3, $4, $5)
    """

takeQuery :: Query
takeQuery = Query """
    delete from sign_in_ticket
    where token_hash = $1 and provider = $2 and created > now() - interval '1 hour'
    returning provider_id as "providerId", email, email_confirmed as "emailConfirmed"
    """

-- | The account a ticket is for, by the id the provider knows it by, and the
-- | address the provider gave for it.
type Ticketed = { providerId :: String, email :: Maybe VouchedEmail }

-- | A ticket for registering the account the provider has vouched for, which
-- | the player shows once they have picked a nickname.
addSignInTicket :: ∀ querier errors. Querier querier =>
    querier -> Provider -> Ticketed -> Async (InternalTerror_ errors) String
addSignInTicket querier provider { providerId, email } = do
    token <- Token.generate
    tokenHash <- Token.hash token
    queryNone querier addQuery
        ( tokenHash
        : ticketName provider
        : providerId
        : toNullable (email <#> _.email)
        :| maybe false _.confirmed email
        )
    pure $ unwrap token

-- | The account the provider's ticket is for, which the ticket is then spent
-- | on. One past its hour, spent, or another provider's is `spent`.
takeSignInTicket :: ∀ querier body errors. Querier querier =>
    querier -> Provider -> body -> String
    -> Async (TerrorVar (InternalRow_ + BadRequestRow body + errors)) Ticketed
takeSignInTicket querier provider spent ticket = do
    tokenHash <- Token.hash (Token.Token ticket)
    row :: Maybe { providerId :: String, email :: Maybe String, emailConfirmed :: Boolean } <-
        queryFirstMaybe querier takeQuery (tokenHash :| ticketName provider)
    { providerId, email, emailConfirmed } <- row # note (Terror (badRequest_ spent)
        [ "The " <> ticketName provider <> " ticket is spent or past its hour." ])
    pure { providerId, email: email <#> { email: _, confirmed: emailConfirmed } }
