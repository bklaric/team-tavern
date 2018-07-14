module TeamTavern.Infrastructure.EnsureNotSignedIn.Run where

import Prelude

import Async (Async, fromEither)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), Variant, inj)
import TeamTavern.Infrastructure.Cookie (lookupCookie)

type EnsureNotSignedInError = { token :: String }

ensureNotSignedIn
    :: forall errors
    .  Map String String
    -> Async
        (Variant (ensureNotSignedIn :: EnsureNotSignedInError | errors))
        Unit
ensureNotSignedIn cookies =
    lookupCookie cookies
    # case _ of
        Nothing -> Right unit
        Just token ->
            Left $ inj (SProxy :: SProxy "ensureNotSignedIn") { token }
    # fromEither
