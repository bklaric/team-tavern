module TeamTavern.Server.Player.Infrastructure.SendConfirmation
    ( Confirmation
    , ConfirmationLimits
    , addConfirmation
    , createConfirmationLimits
    , sendConfirmation
    , trySendConfirmation
    ) where

import Prelude

import Async (Async, fromEffect)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll, split, toLower)
import Data.String.CodeUnits (drop, lastIndexOf, take)
import Effect (Effect)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Server.Infrastructure.Email (Block(..), Email, Mailer, sendEmail)
import TeamTavern.Server.Infrastructure.GenerateNonce (Nonce, generateNonce, toString)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.RateLimit (Admission(..), Limiter, admitAll, createLimiter)
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

-- | Anyone can give any address, and an email change, a sign-up or a sign-in
-- | each send it a link, so a mailbox gets 3 an hour and 10 a day, however
-- | many accounts give it.
type ConfirmationLimits = { perHour :: Limiter, perDay :: Limiter }

createConfirmationLimits :: Effect ConfirmationLimits
createConfirmationLimits = do
    perHour <- createLimiter "Confirmation links to one mailbox an hour" { count: 3, seconds: 3600 }
    perDay <- createLimiter "Confirmation links to one mailbox a day" { count: 10, seconds: 86400 }
    pure { perHour, perDay }

-- The mailbox an address reaches, which differently written addresses share:
-- case aside, without a `+tag`, which most providers deliver to the address
-- without it, and for Gmail, which ignores them, without dots.
mailbox :: String -> String
mailbox email = let
    address = toLower email
    in
    case lastIndexOf (Pattern "@") address of
        Nothing -> address
        Just at -> let
            local = take at address # split (Pattern "+") # Array.head # fromMaybe ""
            domain = drop (at + 1) address
            in
            if domain == "gmail.com" || domain == "googlemail.com"
            then replaceAll (Pattern ".") (Replacement "") local <> "@gmail.com"
            else local <> "@" <> domain

-- | Sends the link unless its mailbox has had all it gets, and answers which.
-- | A link held back isn't sent, and the request that set it off goes on:
-- | the address is changed or the account made either way, and the account
-- | page sends the link again. A failed send is logged and doesn't fail the
-- | request either.
trySendConfirmation :: ∀ left. ConfirmationLimits -> Mailer -> Confirmation -> Async left Admission
trySendConfirmation { perHour, perDay } mailer confirmation = do
    admission <- fromEffect $ admitAll [ perHour, perDay ] $ mailbox confirmation.email
    case admission of
        Admitted -> sendEmail mailer "Error sending email confirmation" $ confirmationEmail confirmation
        Refused _ -> pure unit
    pure admission

-- | As `trySendConfirmation`, for a caller that goes on either way.
sendConfirmation :: ∀ left. ConfirmationLimits -> Mailer -> Confirmation -> Async left Unit
sendConfirmation limits mailer = void <<< trySendConfirmation limits mailer
