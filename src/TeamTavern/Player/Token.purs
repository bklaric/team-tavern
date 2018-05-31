module TeamTavern.Player.Token (Token, create) where

import Prelude

import Data.Either (Either)
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Effect (Effect)
import Node.Buffer (Buffer, toString__)
import Node.Crypto (randomBytes)
import Node.Encoding (Encoding(..))
import Node.Errors (Error)

newtype Token = Token String

derive instance newtypeToken :: Newtype Token _

tokenByteSize :: Int
tokenByteSize = 20

bufferToToken :: Buffer -> Effect Token
bufferToToken buffer = buffer # toString__ Hex <#> Token

create :: (Either Error Token -> Effect Unit) -> Effect Unit
create callback =
    randomBytes tokenByteSize (traverse bufferToToken >=> callback)
