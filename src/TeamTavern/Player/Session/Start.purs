module TeamTavern.Player.Session.Start (StartF(..), start) where

import Prelude

import Data.Symbol (SProxy(..))
import Run (FProxy, Run, lift)
import TeamTavern.Infrastructure.EnsureNotSignedIn (EnsureNotSignedInF, ensureNotSignedIn)
import TeamTavern.Player.Domain.Types (NicknamedNonce, IdentifiedToken)

data StartF result
    = ReadNicknamedNonce (NicknamedNonce -> result)
    | ConsumeToken NicknamedNonce (IdentifiedToken -> result)

derive instance functorSignInF :: Functor StartF

_start = SProxy :: SProxy "start"

readNicknamedNonce :: forall run.
    Run (start :: FProxy StartF | run) NicknamedNonce
readNicknamedNonce = lift _start (ReadNicknamedNonce identity)

consumeToken :: forall run.
    NicknamedNonce -> Run (start :: FProxy StartF | run) IdentifiedToken
consumeToken nonce = lift _start (ConsumeToken nonce identity)

start :: forall run. Run
    ( ensureNotSignedIn :: FProxy EnsureNotSignedInF
    , start :: FProxy StartF
    | run)
    IdentifiedToken
start = do
    ensureNotSignedIn
    nonce <- readNicknamedNonce
    consumeToken nonce
