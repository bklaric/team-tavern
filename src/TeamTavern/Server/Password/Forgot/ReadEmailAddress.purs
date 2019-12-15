module TeamTavern.Server.Password.Forgot.ReadEmailAddress where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (labelMap)
import Data.Variant (SProxy(..), Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Simple.JSON.Async (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)

type Email = String

type ReadEmailAddressError errors = Variant
    ( unreadableEmailAddress ::
        { content :: String
        , errors :: MultipleErrors
        }
    | errors )

readEmailAddress :: forall errors.
    Body -> Async (ReadEmailAddressError errors) Email
readEmailAddress body = do
    content <- readBody body
    { email } :: { email :: Email } <- readJSON content # labelMap
        (SProxy :: SProxy "unreadableEmailAddress") { content, errors: _ }
    pure email
