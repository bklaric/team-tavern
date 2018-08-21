module TeamTavern.Player.Session.Prepare (PrepareF(..), prepare) where

import Prelude

import Data.Symbol (SProxy(..))
import Run (FProxy, Run, lift)
import TeamTavern.Infrastructure.EnsureNotSignedIn (EnsureNotSignedInF, ensureNotSignedIn)
import TeamTavern.Player.Domain.Email (Email)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Types (Credentials, NoncedIdentifiers, Secrets, Identifiers)
import TeamTavern.Player.Infrastructure.Types (IdentifiersModel)

data PrepareF result
    = ReadIdentifiers (IdentifiersModel -> result)
    | ValidateIdentifiers IdentifiersModel (Identifiers -> result)
    | GenerateSecrets Email Nickname (Secrets -> result)
    | CreateSession Credentials result
    | NotifyPlayer NoncedIdentifiers result

derive instance functor :: Functor PrepareF

_prepare = SProxy :: SProxy "prepare"

readIdentifiers :: forall run.
    Run (prepare :: FProxy PrepareF | run) IdentifiersModel
readIdentifiers = lift _prepare (ReadIdentifiers identity)

validateIdentifiers :: forall run.
    IdentifiersModel -> Run (prepare :: FProxy PrepareF | run) Identifiers
validateIdentifiers model = lift _prepare (ValidateIdentifiers model identity)

generateSecrets :: forall run.
    Email -> Nickname -> Run (prepare :: FProxy PrepareF | run) Secrets
generateSecrets email nickname =
    lift _prepare (GenerateSecrets email nickname identity)

createSession :: forall run.
    Credentials -> Run (prepare :: FProxy PrepareF | run) Unit
createSession credentials = lift _prepare (CreateSession credentials unit)

notifyPlayer :: forall run.
    NoncedIdentifiers -> Run (prepare :: FProxy PrepareF | run) Unit
notifyPlayer identifiers =
    lift _prepare (NotifyPlayer identifiers unit)

prepare :: forall run. Run
    ( ensureNotSignedIn :: FProxy EnsureNotSignedInF
    , prepare :: FProxy PrepareF
    | run)
    Unit
prepare = do
    ensureNotSignedIn
    identifiersModel <- readIdentifiers
    { email, nickname } <- validateIdentifiers identifiersModel
    { token, nonce } <- generateSecrets email nickname
    createSession { email, nickname, token, nonce }
    notifyPlayer { email, nickname, nonce }
