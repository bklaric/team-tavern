module TeamTavern.Game.Infrastructure.ReadHandle where

import Prelude

import Async (Async)
import Async as Async
import Data.Bifunctor.Label (labelMap)
import Data.List.Types (NonEmptyList)
import Data.Variant (SProxy(..), Variant)
import TeamTavern.Game.Domain.Handle (Handle, HandleError)
import TeamTavern.Game.Domain.Handle as Handle

type ReadHandleError errors = Variant
    ( invalidHandle ::
        { handle :: String
        , errors :: NonEmptyList HandleError
        }
    | errors )

readHandle :: forall errors. String -> Async (ReadHandleError errors) Handle
readHandle handle =
    handle
    # Handle.create'
    # labelMap (SProxy :: SProxy "invalidHandle") { handle, errors: _ }
    # Async.fromEither
