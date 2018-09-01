module TeamTavern.Player.Session.Prepare.GenerateSecrets where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Variant (Variant)
import Data.Bifunctor.Label (label)
import TeamTavern.Player.Domain.Email (Email)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Types (Secrets)
import TeamTavern.Player.Infrastructure.GenerateSecrets as Infrastructure

type GenerateSecretsError =
    { error :: Infrastructure.GenerateSecretsError
    , email :: Email
    , nickname :: Nickname
    }

generateSecrets
    :: forall errors
    .  Email
    -> Nickname
    -> Async
        (Variant (generateSecrets :: GenerateSecretsError | errors))
        Secrets
generateSecrets email nickname =
    Infrastructure.generateSecrets'
    # lmap { error: _, email, nickname }
    # label Infrastructure._generateSecrets
