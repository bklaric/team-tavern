module TeamTavern.Player.Session.Prepare.GenerateSecrets where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import TeamTavern.Architecture.Async (label)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Types (Secrets)
import TeamTavern.Player.Infrastructure.GenerateSecrets as Infrastructure

type GenerateSecretsError =
    { error :: Infrastructure.GenerateSecretsError
    , nickname :: Nickname
    }

generateSecrets
    :: forall errors
    .  Nickname
    -> Async
        (Variant (generateSecrets :: GenerateSecretsError | errors))
        Secrets
generateSecrets nickname =
    Infrastructure.generateSecrets
    # lmap { error: _, nickname }
    # label (SProxy :: SProxy "generateSecrets")
