module TeamTavern.Server.Player.Infrastructure.SendConfirmation
    (Confirmation, addConfirmation, sendConfirmation) where

import Prelude

import Async (Async)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Server.Infrastructure.Email (Block(..), Email, Mailer, sendEmail)
import TeamTavern.Server.Infrastructure.GenerateNonce (Nonce, generateNonce, toString)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Session.Domain.Token (Token(..))
import TeamTavern.Server.Session.Domain.Token as Token

type Confirmation = { email :: String, nickname :: String, nonce :: Nonce }

-- The link confirms the address it was sent to, so it is stored beside the
-- nonce and a link to an address the player has since changed confirms nothing,
-- and goes as the new one is added.
addConfirmationQuery :: Query
addConfirmationQuery = Query """
    with stale as (
        delete from email_confirmation
        where player_id = $1 and lower(email) <> lower($2)
    )
    insert into email_confirmation (player_id, email, nonce_hash)
    values ($1, $2, $3)
    """

addConfirmation :: ∀ querier errors. Querier querier =>
    querier -> Int -> String -> Async (InternalTerror_ errors) Nonce
addConfirmation querier playerId email = do
    nonce <- generateNonce
    nonceHash <- Token.hash $ Token $ toString nonce
    queryNone querier addConfirmationQuery (playerId : email :| nonceHash)
    pure nonce

confirmationEmail :: Confirmation -> Email
confirmationEmail { email, nickname, nonce } =
    { to: email
    , subject: "Confirm your email"
    , blocks:
        [ Paragraph $ "Hi " <> nickname <> ","
        , Paragraph "Confirm this address for your TeamTavern account:"
        , Button { label: "Confirm email", path: "/confirm-email?nonce=" <> toString nonce }
        , Note $ "Until you do, this is the only email TeamTavern sends it. "
            <> "If you didn't sign up, please ignore this email."
        ]
    , unsubscribe: true
    }

-- | A failed send is logged and doesn't fail the request that sent it: the
-- | account is made either way, and the account page sends the link again.
sendConfirmation :: ∀ left. Mailer -> Confirmation -> Async left Unit
sendConfirmation mailer confirmation =
    sendEmail mailer "Error sending email confirmation" $ confirmationEmail confirmation
