module TeamTavern.Player.SignIn.Run where

import Prelude

import Async (Async, alwaysRight)
import Data.Map (Map)
import Data.String.NonEmpty (NonEmptyString)
import Data.Variant (match)
import MultiMap (empty)
import Perun.Request.Body (Body)
import Perun.Response (Response)
import Postgres.Pool (Pool)
import Run (interpret)
import Run as VariantF
import Simple.JSON (writeJSON)
import TeamTavern.Architecture.Async (examineErrorWith)
import TeamTavern.Infrastructure.Cookie (setCookieHeader)
import TeamTavern.Infrastructure.EnsureNotSignedIn (EnsureNotSignedInF(..))
import TeamTavern.Infrastructure.EnsureNotSignedIn.Run (ensureNotSignedIn)
import TeamTavern.Player.Domain.PlayerId (PlayerId)
import TeamTavern.Player.SignIn (SignInF(..), signIn)
import TeamTavern.Player.SignIn.ConsumeToken (consumeToken)
import TeamTavern.Player.SignIn.Types.Error (SignInError, logError)
import TeamTavern.Player.SignIn.Types.ErrorModel (fromSignInError)
import TeamTavern.Player.SignIn.ReadNicknamedNonce (readNicknamedNonce)
import TeamTavern.Player.Domain.Token (Token)

interpretSignIn
    :: Pool
    -> NonEmptyString
    -> Map String String
    -> Body
    -> Async SignInError { id :: PlayerId, token :: Token }
interpretSignIn pool nickname cookies body = signIn # interpret (VariantF.match
    { ensureNotSignedIn: case _ of
        EnsureNotSignedIn send ->
            ensureNotSignedIn cookies <#> const send
    , signIn: case _ of
        ReadNicknamedNonce send ->
            readNicknamedNonce nickname body <#> send
        ConsumeToken nonce send ->
            consumeToken pool nonce <#> send
    })

errorResponse :: SignInError -> Response
errorResponse error = let
    errorModel = fromSignInError error
    in
    errorModel # match
        { signedIn: const $
            { statusCode: 403
            , headers: empty
            , content: writeJSON errorModel
            }
        , invalidNickname: const $
            { statusCode: 404
            , headers: empty
            , content: writeJSON errorModel
            }
        , invalidNonce: const $
            { statusCode: 400
            , headers: empty
            , content: writeJSON errorModel
            }
        , noTokenToConsume: const $
            { statusCode: 400
            , headers: empty
            , content: writeJSON errorModel
            }
        , other: const $
            { statusCode: 500
            , headers: empty
            , content: writeJSON errorModel
            }
        }

successResponse :: { id :: PlayerId, token :: Token} -> Response
successResponse { id, token } =
    { statusCode: 204
    , headers: setCookieHeader id token
    , content: mempty
    }

respondSignIn
    :: Async SignInError { id :: PlayerId, token :: Token}
    -> (forall left. Async left Response)
respondSignIn = alwaysRight errorResponse successResponse

handleSignIn
    :: Pool
    -> NonEmptyString
    -> Map String String
    -> Body
    -> (forall left. Async left Response)
handleSignIn pool nickname cookies body =
    interpretSignIn pool nickname cookies body
    # examineErrorWith logError
    # respondSignIn
