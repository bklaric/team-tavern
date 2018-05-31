module TeamTavern.Player.Register where

import Prelude

import Data.Variant (SProxy(..))
import Run (FProxy, Run, lift)
import TeamTavern.Player.Credentials (Credentials)
import TeamTavern.Player.Identifiers (IdentifiersModel, Identifiers)
import TeamTavern.Player.Token (Token)

data RegisterF result
    = ReadIdentifiers (IdentifiersModel -> result)
    | ValidateIdentifiers IdentifiersModel (Identifiers -> result)
    | GenerateToken (Token -> result)
    | AddPlayer Credentials result
    | SendEmail Credentials result

derive instance functorRegisterF :: Functor RegisterF

_register = SProxy :: SProxy "register"

readIdentifiers :: forall computations.
    Run (register :: FProxy RegisterF | computations) IdentifiersModel
readIdentifiers = lift _register (ReadIdentifiers id)

validateIdentifiers
    :: forall computations
    .  IdentifiersModel
    -> Run (register :: FProxy RegisterF | computations) Identifiers
validateIdentifiers model = lift _register (ValidateIdentifiers model id)

generateToken :: forall computations.
    Run (register :: FProxy RegisterF | computations) Token
generateToken = lift _register (GenerateToken id)

addPlayer :: forall computations.
    Credentials -> Run (register :: FProxy RegisterF | computations) Unit
addPlayer credentials = lift _register (AddPlayer credentials unit)

sendEmail :: forall computations.
    Credentials -> Run (register :: FProxy RegisterF | computations) Unit
sendEmail credentials = lift _register (SendEmail credentials unit)

register :: forall computations.
    Run (register :: FProxy RegisterF | computations) Credentials
register = do
    model <- readIdentifiers
    { email, nickname } <- validateIdentifiers model
    token <- generateToken
    let credentials = { email, nickname, token }
    addPlayer credentials
    sendEmail credentials
    pure credentials
