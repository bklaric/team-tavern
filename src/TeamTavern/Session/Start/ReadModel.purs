module TeamTavern.Session.Start.ReadModel where

import Prelude

import Async (Async)
import Data.Bifunctor.Label (labelMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Foreign (MultipleErrors)
import Perun.Request.Body (Body)
import Simple.JSON.Async (readJSON)
import TeamTavern.Architecture.Perun.Request.Body (readBody)

newtype NicknameOrEmail = NicknameOrEmail String

derive instance newtypeNicknameOrString :: Newtype NicknameOrEmail _

derive instance genericNicknameOrEmail :: Generic NicknameOrEmail _

instance showNicknameOrEmail :: Show NicknameOrEmail where show = genericShow

newtype Password = Password String

derive instance newtypePassword :: Newtype Password _

newtype Nonce = Nonce String

derive instance newtypeNonce :: Newtype Nonce _

derive instance genericNonce :: Generic Nonce _

instance showNonce :: Show Nonce where show = genericShow

type StartDto =
    { nicknameOrEmail :: String
    , password :: String
    , nonce :: Maybe String
    }

type StartModel =
    { nicknameOrEmail :: NicknameOrEmail
    , password :: Password
    , nonce :: Maybe Nonce
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
    model @ { nicknameOrEmail, password, nonce } :: StartDto <- readJSON content
        # labelMap (SProxy :: SProxy "unreadableDto") { content, errors: _ }
    pure
        { nicknameOrEmail: NicknameOrEmail nicknameOrEmail
        , password: Password password
        , nonce: Nonce <$> nonce
        }
