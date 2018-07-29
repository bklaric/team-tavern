module TeamTavern.Player.Session.Prepare (PrepareF(..), prepare) where

import Prelude

import Data.Symbol (SProxy(..))
import Run (FProxy, Run, lift)
import TeamTavern.Infrastructure.EnsureNotSignedIn (EnsureNotSignedInF, ensureNotSignedIn)
import TeamTavern.Player.Domain.Email (Email)
import TeamTavern.Player.Domain.Nickname (Nickname)
import TeamTavern.Player.Domain.Types (NicknamedSecrets, Secrets, NoncedIdentifiers)

data PrepareF result
    = ReadNickname (Nickname -> result)
    | GenerateSecrets Nickname (Secrets -> result)
    | CreateSession NicknamedSecrets (Email -> result)
    | NotifyPlayer NoncedIdentifiers result

derive instance functor :: Functor PrepareF

_prepare = SProxy :: SProxy "prepare"

readNickname :: forall run.
    Run (prepare :: FProxy PrepareF | run) Nickname
readNickname = lift _prepare (ReadNickname identity)

generateSecrets :: forall run.
    Nickname -> Run (prepare :: FProxy PrepareF | run) Secrets
generateSecrets nickname = lift _prepare (GenerateSecrets nickname identity)

createSession :: forall run.
    NicknamedSecrets -> Run (prepare :: FProxy PrepareF | run) Email
createSession secrets =
    lift _prepare (CreateSession secrets identity)

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
    nickname <- readNickname
    { token, nonce } <- generateSecrets nickname
    email <- createSession { nickname, token, nonce }
    notifyPlayer { nickname, email, nonce }
