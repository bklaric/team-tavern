module TeamTavern.Player.Session.Prepare.Run.Types where

import Data.Variant (Variant)
import TeamTavern.Infrastructure.EnsureNotSignedIn.Run (EnsureNotSignedInError)
import TeamTavern.Player.Infrastructure (ReadNicknameError)
import TeamTavern.Player.Session.Prepare.CreateSession (CreateSessionError)
import TeamTavern.Player.Session.Prepare.GenerateSecrets (GenerateSecretsError)
import TeamTavern.Player.Session.Prepare.NotifyPlayer (NotifyPlayerError)

type PrepareError = Variant
    ( ensureNotSignedIn :: EnsureNotSignedInError
    , readNickname :: ReadNicknameError
    , generateSecrets :: GenerateSecretsError
    , createSession :: CreateSessionError
    , notifyPlayer :: NotifyPlayerError
    )
