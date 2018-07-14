module TeamTavern.Player.SignIn.Run where

import Prelude

import Async (Async, alwaysRight)
import Data.Map (Map)
import Data.Monoid (mempty)
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
import TeamTavern.Player.SignIn (SignInF(..), signIn)
import TeamTavern.Player.SignIn.ConsumeToken (consumeToken)
import TeamTavern.Player.SignIn.Error (SignInError, logError)
import TeamTavern.Player.SignIn.ErrorModel (fromSignInError)
import TeamTavern.Player.SignIn.ReadNickname (readNickname)
import TeamTavern.Player.SignIn.ReadToken (readToken)
import TeamTavern.Player.Token (Token)

interpretSignIn
    :: Pool
    -> NonEmptyString
    -> Map String String
    -> Body
    -> Async SignInError Token
interpretSignIn pool nickname' cookies body = signIn # interpret (VariantF.match
    { ensureNotSignedIn: case _ of
        EnsureNotSignedIn send ->
            ensureNotSignedIn cookies <#> const send
    , signIn: case _ of
        ReadNickname send ->
            readNickname nickname' <#> send
        ReadToken send ->
            readToken body <#> send
        ConsumeToken nickname token send ->
            consumeToken pool nickname token <#> const send
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
        , invalidToken: const $
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

successResponse :: Token -> Response
successResponse token =
    { statusCode: 204
    , headers: setCookieHeader token
    , content: mempty
    }

respondSignIn ::
    Async SignInError Token -> (forall left. Async left Response)
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
