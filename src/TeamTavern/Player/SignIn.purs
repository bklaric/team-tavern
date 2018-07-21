module TeamTavern.Player.SignIn where

import Prelude

import Data.Symbol (SProxy(..))
import Run (FProxy, Run, lift)
import TeamTavern.Infrastructure.EnsureNotSignedIn (EnsureNotSignedInF, ensureNotSignedIn)
import TeamTavern.Player.Nickname (Nickname)
import TeamTavern.Player.Token (Token)

data SignInF result
    = ReadNickname (Nickname -> result)
    | ReadToken (Token -> result)
    | ConsumeToken Nickname Token result

derive instance functorSignInF :: Functor SignInF

_signIn = SProxy :: SProxy "signIn"

readNickname :: forall run. Run (signIn :: FProxy SignInF | run) Nickname
readNickname = lift _signIn (ReadNickname identity)

readToken :: forall run. Run (signIn :: FProxy SignInF | run) Token
readToken = lift _signIn (ReadToken identity)

consumeToken :: forall run.
    Nickname -> Token -> Run (signIn :: FProxy SignInF | run) Unit
consumeToken nickname token = lift _signIn (ConsumeToken nickname token unit)

signIn :: forall run. Run
    ( ensureNotSignedIn :: FProxy EnsureNotSignedInF
    , signIn :: FProxy SignInF
    | run)
    Token
signIn = do
    ensureNotSignedIn
    nickname <- readNickname
    token <- readToken
    consumeToken nickname token
    pure token
