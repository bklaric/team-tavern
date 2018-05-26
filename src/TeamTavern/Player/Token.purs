module TeamTavern.Player.Token (Token, create) where

import Prelude

import Async (Async, fromEitherCont)
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

create :: Async Error Token
create = fromEitherCont \callback ->
    randomBytes tokenSize (traverse bufferToToken >=> callback)
