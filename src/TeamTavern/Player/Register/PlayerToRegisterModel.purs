module TeamTavern.Player.Register.PlayerToRegisterModel where

import Prelude

import Architecture.Async (label)
import Async (Async, fromEither)
import Data.Foreign (ForeignError)
import Data.List.Types (NonEmptyList)
import Data.Variant (SProxy(..), Variant)
import Simple.JSON (readJSON)

type PlayerToRegisterModel fields =
    { email :: String
    , nickname :: String
    | fields
    }

readPlayerToRegisterModel :: forall errors.
    String
    -> Async
        (Variant (model :: NonEmptyList ForeignError | errors))
        { email :: String, nickname :: String }
readPlayerToRegisterModel string =
    readJSON string # fromEither # label (SProxy :: SProxy "model")
