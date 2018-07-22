module TeamTavern.Player.SignIn
    ( SignInF(..)
    , signIn
    ) where

import Prelude

import Data.Symbol (SProxy(..))
import Run (FProxy, Run, lift)
import TeamTavern.Infrastructure.EnsureNotSignedIn (EnsureNotSignedInF, ensureNotSignedIn)
import TeamTavern.Player.SignIn.Types.IdentifiedToken (IdentifiedToken)
import TeamTavern.Player.SignIn.Types.NicknamedNonce (NicknamedNonce)

data SignInF result
    = ReadNicknamedNonce (NicknamedNonce -> result)
    | ConsumeToken NicknamedNonce (IdentifiedToken -> result)

derive instance functorSignInF :: Functor SignInF

_signIn = SProxy :: SProxy "signIn"

readNicknamedNonce :: forall run.
    Run (signIn :: FProxy SignInF | run) NicknamedNonce
readNicknamedNonce = lift _signIn (ReadNicknamedNonce identity)

consumeToken :: forall run.
    NicknamedNonce -> Run (signIn :: FProxy SignInF | run) IdentifiedToken
consumeToken nonce = lift _signIn (ConsumeToken nonce identity)

signIn :: forall run. Run
    ( ensureNotSignedIn :: FProxy EnsureNotSignedInF
    , signIn :: FProxy SignInF
    | run)
    IdentifiedToken
signIn = do
    ensureNotSignedIn
    nonce <- readNicknamedNonce
    consumeToken nonce
