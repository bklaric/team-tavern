module TeamTavern.Player.Session.Prepare.ReadIdentifiers where

import Prelude

import Async (Async, fromEither)
import Data.Bifunctor (lmap)
import Data.List.Types (NonEmptyList)
import Data.String.NonEmpty (NonEmptyString, toString)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Foreign (ForeignError)
import Perun.Request.Body (Body)
import Simple.JSON (readJSON)
import TeamTavern.Architecture.Async (label)
import TeamTavern.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Player.Infrastructure.Types (IdentifiersModel)

type ReadIdentifiersError =
    { errors :: NonEmptyList ForeignError
    , body :: String
    , nickname :: NonEmptyString
    }

readIdentifiers
    :: forall errors
    .  NonEmptyString
    -> Body
    -> Async
        (Variant (readIdentifiers :: ReadIdentifiersError | errors))
        IdentifiersModel
readIdentifiers nickname body = label (SProxy :: SProxy "readIdentifiers") do
    content <- readBody body
    ({ email } :: { email :: String }) <- readJSON content
        # lmap { errors: _, body: content, nickname }
        # fromEither
    pure { email, nickname: toString nickname }
