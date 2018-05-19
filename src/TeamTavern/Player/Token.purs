module TeamTavern.Player.Token (Token, create) where

import Prelude

import Control.Monad.Cont (ContT(..))
import Data.Either (Either)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Effect (Effect)
import Node.Buffer (Buffer, toString___)
import Node.Crypto (randomBytes)
import Node.Errors (Error)

newtype Token = Token String

derive instance newtypeToken :: Newtype Token _

tokenSize :: Int
tokenSize = 40

bufferToToken :: Buffer -> Effect Token
bufferToToken buffer = buffer # toString___ <#> Token

create' :: (Either Error Token -> Effect Unit) -> Effect Unit
create' callback = randomBytes tokenSize (traverse bufferToToken >=> callback)

create :: ContT Unit Effect (Either Error Token)
create = ContT create'
