module TeamTavern.Player.Session.Prepare.NotifyPlayer
    (NotifyPlayerError, notifyPlayer) where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Postmark.Client (Client)
import Postmark.Error (Error)
import Postmark.Message (Message)
import Data.Bifunctor.Label (label)
import Postmark.Async.Client (sendEmail)
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

type NotifyPlayerError =
    { identifiers :: NoncedIdentifiers
    , error :: Error
    }

notifyPlayer
    :: forall errors
    .  Client
    -> NoncedIdentifiers
    -> Async (Variant (notifyPlayer :: NotifyPlayerError | errors)) Unit
notifyPlayer client identifiers =
    client
    # sendEmail (message identifiers)
    # lmap { error: _, identifiers }
    # label (SProxy :: SProxy "notifyPlayer")
