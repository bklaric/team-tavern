module TeamTavern.Session.Prepare.SendSignInEmail
    (SendSignInEmailError, sendSignInEmail) where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (labelMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Effect.Class.Console (logShow)
import Postmark.Async.Client (sendEmail)
import Postmark.Client (Client)
import Postmark.Error (Error)
import Postmark.Message (Message)
import TeamTavern.Player.Domain.Types (NoncedIdentifiers)

message :: NoncedIdentifiers -> Message
message { nickname, email, nonce } =
    { to: unwrap email
    , from: "branimir.klaric1@xnet.hr"
    , subject: toNullable $ Just "TeamTavern sign in"
    , textBody: toNullable $ Just $
        "Hi " <> unwrap nickname <> ",\n\n"
        <> "Your sign in code is " <> unwrap nonce <> "."
    }

type SendSignInEmailError errors = Variant
    ( sendEmailError ::
        { message :: Message
        , error :: Error
        }
    | errors )

sendSignInEmail
    :: forall errors
    .  Maybe Client
    -> NoncedIdentifiers
    -> Async (SendSignInEmailError errors) Unit
sendSignInEmail client identifiers = let
    message' = message identifiers
    in
    case client of
        Nothing -> logShow message'
        Just client' ->
            client'
            # sendEmail message'
            # labelMap (SProxy :: SProxy "sendEmailError")
            { message: message', error: _ }
