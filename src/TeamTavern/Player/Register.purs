module TeamTavern.Player.Register where

import Prelude

import Data.Variant (SProxy(..))
import Run (FProxy, Run, lift)
import TeamTavern.Player.Credentials (Credentials)
import TeamTavern.Player.Identifiers (IdentifiersModel, Identifiers)
import TeamTavern.Player.Token (Token)

data RegisterF result
    = EnsureNotSignedIn result
    | ReadIdentifiers (IdentifiersModel -> result)
    | ValidateIdentifiers IdentifiersModel (Identifiers -> result)
    | GenerateToken Identifiers (Token -> result)
    | AddPlayer Credentials result
    | SendEmail Credentials result

derive instance functorRegisterF :: Functor RegisterF

_register = SProxy :: SProxy "register"

ensureNotSignedIn :: forall run. Run (register :: FProxy RegisterF | run) Unit
ensureNotSignedIn = lift _register (EnsureNotSignedIn unit)

readIdentifiers :: forall run.
    Run (register :: FProxy RegisterF | run) IdentifiersModel
readIdentifiers = lift _register (ReadIdentifiers id)

validateIdentifiers :: forall run.
    IdentifiersModel -> Run (register :: FProxy RegisterF | run) Identifiers
validateIdentifiers model = lift _register (ValidateIdentifiers model id)

generateToken :: forall run.
    Identifiers -> Run (register :: FProxy RegisterF | run) Token
generateToken identifiers = lift _register (GenerateToken identifiers id)

addPlayer :: forall run.
    Credentials -> Run (register :: FProxy RegisterF | run) Unit
addPlayer credentials = lift _register (AddPlayer credentials unit)

sendEmail :: forall run.
    Credentials -> Run (register :: FProxy RegisterF | run) Unit
sendEmail credentials = lift _register (SendEmail credentials unit)

register :: forall run. Run (register :: FProxy RegisterF | run) Credentials
register = do
    ensureNotSignedIn
    model <- readIdentifiers
    { email, nickname } <- validateIdentifiers model
    token <- generateToken { email, nickname }
    let credentials = { email, nickname, token }
    addPlayer credentials
    sendEmail credentials
    pure credentials
