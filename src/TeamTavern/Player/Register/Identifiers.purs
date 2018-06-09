module TeamTavern.Player.Register.Identifiers where

import Prelude

import Async (Async, fromEither)
import Data.Bifunctor (lmap)
import Data.Foreign (ForeignError)
import Data.List.Types (NonEmptyList)
import Data.Variant (SProxy(..), Variant)
import Perun.Request.Body (Body)
import Simple.JSON (readJSON)
import TeamTavern.Architecture.Async (label)
import TeamTavern.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Player.Identifiers (IdentifiersError, IdentifiersModel, Identifiers, create)
import Validated (toEither)

type ModelError = { errors :: NonEmptyList ForeignError, body :: String }

readIdentifiers :: forall errors.
    Body -> Async (Variant (model :: ModelError | errors)) IdentifiersModel
readIdentifiers body = label (SProxy :: SProxy "model") do
    content <- readBody body
    readJSON content # fromEither # lmap { errors: _, body: content }

type ValidationError =
    { errors :: NonEmptyList IdentifiersError
    , model :: IdentifiersModel
    }

validateIdentifiers
    :: forall errors
    .  IdentifiersModel
    -> Async (Variant (validation :: ValidationError | errors)) Identifiers
validateIdentifiers model =
    create model
    # toEither
    # fromEither
    # lmap { errors: _, model }
    # label (SProxy :: SProxy "validation")
