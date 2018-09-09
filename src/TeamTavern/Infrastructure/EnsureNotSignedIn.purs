module TeamTavern.Infrastructure.EnsureNotSignedIn where

import Prelude

import Async (Async)
import Async as Async
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..), Variant, inj)
import TeamTavern.Infrastructure.Cookie (lookupIdCookie)

type EnsureNotSignedInError errors = Variant
    ( signedIn ::
        { playerId :: String
        , cookies :: Map String String
        }
    | errors )

ensureNotSignedIn :: forall errors.
    Map String String -> Async (EnsureNotSignedInError errors) Unit
ensureNotSignedIn cookies =
    lookupIdCookie cookies
    # case _ of
        Nothing -> Async.right unit
        Just playerId ->
            Async.left $ inj (SProxy :: SProxy "signedIn") { playerId, cookies }
