module TeamTavern.Player.Register
    ( RegisterF(..), register) where

import Prelude

import Data.Variant (SProxy(..))
import Run (FProxy, Run, lift)
import TeamTavern.Infrastructure.EnsureNotSignedIn (EnsureNotSignedInF, ensureNotSignedIn)
import TeamTavern.Player.Domain.Types (Credentials, Identifiers, NoncedIdentifiers, Secrets)
import TeamTavern.Player.Infrastructure.Types (IdentifiersModel)

data RegisterF result
    = ReadIdentifiers (IdentifiersModel -> result)
    | ValidateIdentifiers IdentifiersModel (Identifiers -> result)
    | GenerateSecrets Identifiers (Secrets -> result)
    | AddPlayer Credentials result
    | NotifyPlayer NoncedIdentifiers result

derive instance functorRegisterF :: Functor RegisterF

_register = SProxy :: SProxy "register"

readIdentifiers :: forall run.
    Run (register :: FProxy RegisterF | run) IdentifiersModel
readIdentifiers = lift _register (ReadIdentifiers identity)

validateIdentifiers :: forall run.
    IdentifiersModel -> Run (register :: FProxy RegisterF | run) Identifiers
validateIdentifiers model = lift _register (ValidateIdentifiers model identity)

generateSecrets :: forall run.
    Identifiers -> Run (register :: FProxy RegisterF | run) Secrets
generateSecrets identifiers = lift _register (GenerateSecrets identifiers identity)

addPlayer :: forall run.
    Credentials -> Run (register :: FProxy RegisterF | run) Unit
addPlayer credentials = lift _register (AddPlayer credentials unit)

notifyPlayer :: forall run.
    NoncedIdentifiers -> Run (register :: FProxy RegisterF | run) Unit
notifyPlayer identifiers =
    lift _register (NotifyPlayer identifiers unit)

register :: forall run. Run
    ( ensureNotSignedIn :: FProxy EnsureNotSignedInF
    , register :: FProxy RegisterF
    | run
    )
    Credentials
register = do
    ensureNotSignedIn
    model <- readIdentifiers
    { email, nickname } <- validateIdentifiers model
    { token, nonce } <- generateSecrets { email, nickname }
    let credentials = { email, nickname, token, nonce }
    addPlayer credentials
    notifyPlayer { email, nickname, nonce }
    pure credentials
