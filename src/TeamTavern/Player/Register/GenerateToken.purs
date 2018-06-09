module TeamTavern.Player.Register.GenerateToken where

import Prelude

import Async (Async, fromEitherCont)
import Data.Bifunctor (lmap)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Node.Errors (Error)
import TeamTavern.Architecture.Async (label)
import TeamTavern.Player.Identifiers (Identifiers)
import TeamTavern.Player.Token (Token, create)

type TokenError = { error :: Error, identifiers :: Identifiers }

generateToken :: forall errors.
    Identifiers -> Async (Variant (token :: TokenError | errors)) Token
generateToken identifiers =
    fromEitherCont create
    # lmap { error: _, identifiers }
    # label (SProxy :: SProxy "token")
