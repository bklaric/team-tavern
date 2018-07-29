module TeamTavern.Player.Register.GenerateSecrets where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import TeamTavern.Architecture.Async (label)
import TeamTavern.Player.Domain.Types (Identifiers, Secrets)
import TeamTavern.Player.Infrastructure.GenerateSecrets as Infrastructure

type GenerateSecretsError =
    { error :: Infrastructure.GenerateSecretsError
    , identifiers :: Identifiers
    }

generateSecrets
    :: forall errors
    .  Identifiers
    -> Async
        (Variant (generateSecrets :: GenerateSecretsError | errors))
        Secrets
generateSecrets identifiers =
    Infrastructure.generateSecrets
    # lmap { error: _, identifiers }
    # label (SProxy :: SProxy "generateSecrets")
