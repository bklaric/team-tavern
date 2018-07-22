module TeamTavern.Player.Register
    ( RegisterF(..)
    , register
    ) where

import Prelude

import Data.Variant (SProxy(..))
import Run (FProxy, Run, lift)
import TeamTavern.Infrastructure.EnsureNotSignedIn (EnsureNotSignedInF, ensureNotSignedIn)
import TeamTavern.Player.Register.Types.Credentials (Credentials, IdentifiedCredentials)
import TeamTavern.Player.Register.Types.Identifiers (IdentifiersModel, Identifiers)
import TeamTavern.Player.Register.Types.NoncedToken (NoncedToken)

data RegisterF result
    = ReadIdentifiers (IdentifiersModel -> result)
    | ValidateIdentifiers IdentifiersModel (Identifiers -> result)
    | GenerateToken Identifiers (NoncedToken -> result)
    | AddPlayer Credentials (IdentifiedCredentials -> result)
    | NotifyPlayer IdentifiedCredentials result

derive instance functorRegisterF :: Functor RegisterF

_register = SProxy :: SProxy "register"

readIdentifiers :: forall run.
    Run (register :: FProxy RegisterF | run) IdentifiersModel
readIdentifiers = lift _register (ReadIdentifiers identity)

validateIdentifiers :: forall run.
    IdentifiersModel -> Run (register :: FProxy RegisterF | run) Identifiers
validateIdentifiers model = lift _register (ValidateIdentifiers model identity)

generateToken :: forall run.
    Identifiers -> Run (register :: FProxy RegisterF | run) NoncedToken
generateToken identifiers = lift _register (GenerateToken identifiers identity)

addPlayer
    :: forall run
    .  Credentials
    -> Run (register :: FProxy RegisterF | run) IdentifiedCredentials
addPlayer credentials = lift _register (AddPlayer credentials identity)

notifyPlayer :: forall run.
    IdentifiedCredentials -> Run (register :: FProxy RegisterF | run) Unit
notifyPlayer credentials =
    lift _register (NotifyPlayer credentials unit)

register :: forall run. Run
    ( ensureNotSignedIn :: FProxy EnsureNotSignedInF
    , register :: FProxy RegisterF
    | run
    )
    IdentifiedCredentials
register = do
    ensureNotSignedIn
    model <- readIdentifiers
    { email, nickname } <- validateIdentifiers model
    { token, nonce } <- generateToken { email, nickname }
    credentials <- addPlayer { email, nickname, token, nonce }
    notifyPlayer credentials
    pure credentials
