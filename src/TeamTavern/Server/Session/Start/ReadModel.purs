module TeamTavern.Server.Session.Start.ReadModel where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (labelMap)
import Data.String (trim)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Simple.JSON.Async (readJSON)
import TeamTavern.Server.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Server.Player.Domain.Password (Password(..))
import TeamTavern.Server.Session.Domain.NicknameOrEmail (NicknameOrEmail(..))

type StartDto =
    { nicknameOrEmail :: String
    , password :: String
    }

type StartModel =
    { nicknameOrEmail :: NicknameOrEmail
    , password :: Password
    }

type ReadNonceError errors = Variant
    ( unreadableDto ::
        { content :: String
        , errors :: MultipleErrors
        }
    | errors )

readModel :: forall errors.
    Body -> Async (ReadNonceError errors) StartModel
readModel body = do
    content <- readBody body
    model @ { nicknameOrEmail, password } :: StartDto <- readJSON content
        # labelMap (SProxy :: SProxy "unreadableDto") { content, errors: _ }
    pure
        { nicknameOrEmail: NicknameOrEmail $ trim nicknameOrEmail
        , password: Password password
        }
