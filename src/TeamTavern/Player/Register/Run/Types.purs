module TeamTavern.Player.Register.Run.Types where

import Data.Variant (Variant)
import TeamTavern.Infrastructure.EnsureNotSignedIn.Run (EnsureNotSignedInError)
import TeamTavern.Player.Register.AddPlayer (AddPlayerError)
import TeamTavern.Player.Register.GenerateSecrets (GenerateSecretsError)
import TeamTavern.Player.Register.NotifyPlayer (SendEmailError)
import TeamTavern.Player.Register.ReadIdentifiers (ReadIdentifiersError)
import TeamTavern.Player.Register.ValidateIdentifiers (ValidateIdentifiersError)

type RegisterError = Variant
    ( ensureNotSignedIn :: EnsureNotSignedInError
    , readIdentifiers :: ReadIdentifiersError
    , validateIdentifiers :: ValidateIdentifiersError
    , generateSecrets :: GenerateSecretsError
    , addPlayer :: AddPlayerError
    , sendEmail :: SendEmailError
    )
