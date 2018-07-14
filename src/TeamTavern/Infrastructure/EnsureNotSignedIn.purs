module TeamTavern.Infrastructure.EnsureNotSignedIn where

import Prelude

import Data.Variant (SProxy(..))
import Run (FProxy, Run, lift)

data EnsureNotSignedInF result = EnsureNotSignedIn result

derive instance functorEnsureNotSignedInF :: Functor EnsureNotSignedInF

_ensureNotSignedIn = SProxy :: SProxy "ensureNotSignedIn"

ensureNotSignedIn :: forall run.
    Run (ensureNotSignedIn :: FProxy EnsureNotSignedInF | run) Unit
ensureNotSignedIn = lift _ensureNotSignedIn (EnsureNotSignedIn unit)
