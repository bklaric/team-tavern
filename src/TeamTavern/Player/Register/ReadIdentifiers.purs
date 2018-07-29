module TeamTavern.Player.Register.ReadIdentifiers where

import Prelude

import Async (Async, fromEither)
import Data.Bifunctor (lmap)
import Data.List.Types (NonEmptyList)
import Data.Variant (SProxy(..), Variant)
import Foreign (ForeignError)
import Perun.Request.Body (Body)
import Simple.JSON (readJSON)
import TeamTavern.Architecture.Async (label)
import TeamTavern.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Player.Infrastructure.Types (IdentifiersModel)

type ReadIdentifiersError =
    { errors :: NonEmptyList ForeignError
    , body :: String
    }

readIdentifiers
    :: forall errors
    .  Body
    -> Async
        (Variant (readIdentifiers :: ReadIdentifiersError | errors))
        IdentifiersModel
readIdentifiers body = label (SProxy :: SProxy "readIdentifiers") do
    content <- readBody body
    readJSON content # fromEither # lmap { errors: _, body: content }
