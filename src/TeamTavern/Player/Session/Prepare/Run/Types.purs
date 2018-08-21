module TeamTavern.Player.Session.Prepare.Run.Types where

import Data.Variant (Variant)
import TeamTavern.Infrastructure.EnsureNotSignedIn.Run (EnsureNotSignedInError)
import TeamTavern.Player.Infrastructure.ValidateIdentifiers (ValidateIdentifiersError)
import TeamTavern.Player.Session.Prepare.CreateSession (CreateSessionError)
import TeamTavern.Player.Session.Prepare.GenerateSecrets (GenerateSecretsError)
import TeamTavern.Player.Session.Prepare.NotifyPlayer (NotifyPlayerError)
import TeamTavern.Player.Session.Prepare.ReadIdentifiers (ReadIdentifiersError)

type PrepareError = Variant
    ( ensureNotSignedIn :: EnsureNotSignedInError
    , readIdentifiers :: ReadIdentifiersError
    , validateIdentifiers :: ValidateIdentifiersError
    , generateSecrets :: GenerateSecretsError
    , createSession :: CreateSessionError
    , notifyPlayer :: NotifyPlayerError
    )
