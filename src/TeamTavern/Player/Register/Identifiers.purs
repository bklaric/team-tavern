module TeamTavern.Player.Register.Identifiers where

import Prelude

import Async (Async, fromEither)
import Data.Foreign (ForeignError)
import Data.List.Types (NonEmptyList)
import Data.Variant (SProxy(..), Variant)
import Perun.Request.Body (Body)
import Simple.JSON (readJSON)
import TeamTavern.Architecture.Async (label)
import TeamTavern.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Player.Identifiers (IdentifiersError, IdentifiersModel, Identifiers, create)
import Validated (toEither)

readIdentifiers
    :: forall errors
    .  Body
    -> Async
        (Variant (model :: NonEmptyList ForeignError | errors))
        IdentifiersModel
readIdentifiers =
    readBody
    >=> (readJSON >>> fromEither)
    >>> label (SProxy :: SProxy "model")

validateIdentifiers
    :: forall errors
    .  IdentifiersModel
    -> Async
        (Variant (identifiers :: NonEmptyList IdentifiersError | errors))
        Identifiers
validateIdentifiers =
    create
    >>> toEither
    >>> fromEither
    >>> label (SProxy :: SProxy "identifiers")
