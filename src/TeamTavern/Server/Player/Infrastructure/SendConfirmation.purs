module TeamTavern.Server.Player.Infrastructure.SendConfirmation
    (Confirmation, addConfirmation, sendConfirmation) where

import Prelude

import Async (Async, attempt, fromEffect)
import Data.Either (Either(..))
import Effect.Class.Console (logShow)
import JavaScript.Npm.Pg.Query (class Querier, Query(..), (:), (:|))
import TeamTavern.Server.Infrastructure.Deployment (Deployment(..))
import TeamTavern.Server.Infrastructure.GenerateNonce (Nonce, generateNonce, toString)
import TeamTavern.Server.Infrastructure.Log (logError)
import TeamTavern.Server.Infrastructure.Postgres (queryNone)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.Sendgrid (Message, sendAsync)

type Confirmation = { email :: String, nickname :: String, nonce :: Nonce }

-- The link confirms the address it was sent to, so it is stored beside the
-- nonce and a link to an address the player has since changed confirms nothing.
addConfirmationQuery :: Query
addConfirmationQuery = Query """
    insert into email_confirmation (player_id, email, nonce)
    values ($1, $2, $3)
    """

addConfirmation :: ∀ querier errors. Querier querier =>
    querier -> Int -> String -> Async (InternalTerror_ errors) Nonce
addConfirmation querier playerId email = do
    nonce <- generateNonce
    queryNone querier addConfirmationQuery (playerId : email :| nonce)
    pure nonce

message :: Deployment -> Confirmation -> Message
message deployment { email, nickname, nonce } = let
    link = case deployment of
        -- Only logged, and the development and test stacks serve on different ports.
        Local -> "/confirm-email?nonce=" <> toString nonce
        Cloud -> "https://www.teamtavern.net/confirm-email?nonce=" <> toString nonce
    in
    { to: email
    , from: "admin@teamtavern.net"
    , subject: "Confirm your email"
    , html: "Hi " <> nickname <> ",<br /><br />"
        <> "Open the link below to confirm this address for your TeamTavern account:<br /><br />"
        <> "<a href=\"" <> link <> "\">" <> link <> "</a><br /><br />"
        <> "Until you do, this is the only email TeamTavern sends it. "
        <> "If you didn't sign up, please ignore this email."
    , text: "Hi " <> nickname <> ",\n"
        <> "Open the link below to confirm this address for your TeamTavern account:\n"
        <> link <> "\n"
        <> "Until you do, this is the only email TeamTavern sends it. "
        <> "If you didn't sign up, please ignore this email."
    }

-- | A failed send is logged and doesn't fail the request that sent it: the
-- | account is made either way, and the account page sends the link again.
sendConfirmation :: ∀ left. Deployment -> Confirmation -> Async left Unit
sendConfirmation deployment confirmation = do
    result <- attempt case deployment of
        Local -> logShow $ message deployment confirmation
        Cloud -> sendAsync $ message deployment confirmation
    case result of
        Left error -> fromEffect $ logError "Error sending email confirmation" error
        Right _ -> pure unit
