module TeamTavern.Player.Session.Start.Run (handleStart) where

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
import TeamTavern.Player.Session.Start (StartF(..), start)
import TeamTavern.Player.Session.Start.ConsumeToken (consumeToken)
import TeamTavern.Player.Session.Start.Types.Error (SignInError, logError)
import TeamTavern.Player.Session.Start.Types.ErrorModel (fromSignInError)
import TeamTavern.Player.Session.Start.ReadNicknamedNonce (readNicknamedNonce)
import TeamTavern.Player.Domain.Token (Token)

interpretStart
    :: Pool
    -> NonEmptyString
    -> Map String String
    -> Body
    -> Async SignInError { id :: PlayerId, token :: Token }
interpretStart pool nickname cookies body = start # interpret (VariantF.match
    { ensureNotSignedIn: case _ of
        EnsureNotSignedIn send ->
            ensureNotSignedIn cookies <#> const send
    , start: case _ of
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

respondStart
    :: Async SignInError { id :: PlayerId, token :: Token}
    -> (forall left. Async left Response)
respondStart = alwaysRight errorResponse successResponse

handleStart
    :: Pool
    -> NonEmptyString
    -> Map String String
    -> Body
    -> (forall left. Async left Response)
handleStart pool nickname cookies body =
    interpretStart pool nickname cookies body
    # examineErrorWith logError
    # respondStart
