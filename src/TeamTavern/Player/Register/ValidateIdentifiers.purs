module TeamTavern.Player.Register.ValidateIdentifiers where

import Prelude

import Async (Async, fromEither)
import Data.Bifunctor (lmap)
import Data.List.Types (NonEmptyList)
import Data.Variant (SProxy(..), Variant)
import TeamTavern.Architecture.Async (label)
import TeamTavern.Player.Identifiers (IdentifiersError, IdentifiersModel, Identifiers, create)
import Validated (toEither)

type ValidateIdentifiersError =
    { errors :: NonEmptyList IdentifiersError
    , model :: IdentifiersModel
    }

validateIdentifiers
    :: forall errors
    .  IdentifiersModel
    -> Async
        (Variant (validateIdentifiers :: ValidateIdentifiersError | errors))
        Identifiers
validateIdentifiers model =
    create model
    # toEither
    # fromEither
    # lmap { errors: _, model }
    # label (SProxy :: SProxy "validateIdentifiers")
