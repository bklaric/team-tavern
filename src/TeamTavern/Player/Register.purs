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

ensureNotSignedIn :: forall computations.
    Run (register :: FProxy RegisterF | computations) Unit
ensureNotSignedIn = lift _register (EnsureNotSignedIn unit)

readIdentifiers :: forall computations.
    Run (register :: FProxy RegisterF | computations) IdentifiersModel
readIdentifiers = lift _register (ReadIdentifiers id)

validateIdentifiers
    :: forall computations
    .  IdentifiersModel
    -> Run (register :: FProxy RegisterF | computations) Identifiers
validateIdentifiers model = lift _register (ValidateIdentifiers model id)

generateToken :: forall computations.
    Identifiers -> Run (register :: FProxy RegisterF | computations) Token
generateToken identifiers = lift _register (GenerateToken identifiers id)

addPlayer :: forall computations.
    Credentials -> Run (register :: FProxy RegisterF | computations) Unit
addPlayer credentials = lift _register (AddPlayer credentials unit)

sendEmail :: forall computations.
    Credentials -> Run (register :: FProxy RegisterF | computations) Unit
sendEmail credentials = lift _register (SendEmail credentials unit)

register :: forall computations.
    Run (register :: FProxy RegisterF | computations) Credentials
register = do
    ensureNotSignedIn
    model <- readIdentifiers
    { email, nickname } <- validateIdentifiers model
    token <- generateToken { email, nickname }
    let credentials = { email, nickname, token }
    addPlayer credentials
    sendEmail credentials
    pure credentials
