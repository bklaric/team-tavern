module TeamTavern.Player.SignIn.ReadToken where

import Prelude
 
import Async (Async, fromEither)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Foreign (MultipleErrors)
import Data.List.Types (NonEmptyList)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Perun.Request.Body (Body)
import Simple.JSON (readJSON)
import TeamTavern.Architecture.Async as Async
import TeamTavern.Architecture.Either (label)
import TeamTavern.Architecture.Perun.Request.Body (readBody)
import TeamTavern.Player.Token (Token, TokenError, create)

type ReadTokenError = Variant
    ( invalidBody ::
        { errors :: MultipleErrors, body :: String }
    , invalidToken ::
        { errors :: NonEmptyList TokenError, token :: String }
    )

readToken
    :: forall errors
    .  Body
    -> Async (Variant (readToken :: ReadTokenError | errors)) Token
readToken body = Async.label (SProxy :: SProxy "readToken") do
    content <- readBody body
    case readJSON content of
        Left errors ->
            { errors, body: content }
            # Left
            # label (SProxy :: SProxy "invalidBody")
            # fromEither
        Right token ->
            create token
            # lmap { errors: _, token }
            # label (SProxy :: SProxy "invalidToken")
            # fromEither
