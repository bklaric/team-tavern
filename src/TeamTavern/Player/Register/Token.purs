module TeamTavern.Player.Register.Token where

import Prelude

import Async (Async, fromEitherCont)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Node.Errors (Error)
import TeamTavern.Architecture.Async (label)
import TeamTavern.Player.Token (Token, create)

generateToken :: forall errors. Async (Variant (token :: Error | errors)) Token
generateToken = fromEitherCont create # label (SProxy :: SProxy "token")
