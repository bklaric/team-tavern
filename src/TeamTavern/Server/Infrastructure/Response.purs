module TeamTavern.Server.Infrastructure.Response where

import Prelude

import Data.Variant (match)
import Perun.Response (Response, badRequest__, forbidden__, internalServerError__, notFound__, ok_, unauthorized__)
import Prim.Row (class Lacks)
import Record.Builder (Builder)
import Record.Builder as Builder
import TeamTavern.Server.Infrastructure.Error (CommonError)
import Type.Proxy (Proxy(..))
import Yoga.JSON (class WriteForeign, writeJSON)

okJson_ :: forall content. WriteForeign content => content -> Response
okJson_ content = ok_$ writeJSON content

-- Errors

internalResponseHandler :: forall fields. Lacks "internal" fields =>
    Builder (Record fields) { internal :: Array String -> Response | fields }
internalResponseHandler = Builder.insert (Proxy :: _ "internal") $ const internalServerError__

notFoundResponseHandler :: forall fields. Lacks "notFound" fields =>
    Builder (Record fields) { notFound :: Array String -> Response | fields }
notFoundResponseHandler = Builder.insert (Proxy :: _ "notFound") $ const notFound__

notAuthenticatedResponseHandler :: forall fields. Lacks "notAuthenticated" fields =>
    Builder (Record fields) { notAuthenticated :: Array String -> Response | fields }
notAuthenticatedResponseHandler = Builder.insert (Proxy :: _ "notAuthenticated") $ const unauthorized__

notAuthorizedResponseHandler :: forall fields. Lacks "notAuthorized" fields =>
    Builder (Record fields) { notAuthorized :: Array String -> Response | fields }
notAuthorizedResponseHandler = Builder.insert (Proxy :: _ "notAuthorized") $ const forbidden__

clientResponseHandler :: forall handlers. Lacks "client" handlers =>
    Builder (Record handlers) { client :: Array String -> Response | handlers }
clientResponseHandler = Builder.insert (Proxy :: _ "client") $ const badRequest__

commonResponseHandler
    :: forall handlers
    .  Lacks "internal" handlers
    => Lacks "notFound" handlers
    => Lacks "notAuthenticated" handlers
    => Lacks "notAuthorized" handlers
    => Lacks "client" handlers
    => Builder (Record handlers)
        { client :: Array String -> Response
        , internal :: Array String -> Response
        , notAuthenticated :: Array String -> Response
        , notAuthorized :: Array String -> Response
        , notFound :: Array String -> Response
        | handlers
        }
commonResponseHandler =
    internalResponseHandler
    >>> notFoundResponseHandler
    >>> notAuthenticatedResponseHandler
    >>> notAuthorizedResponseHandler
    >>> clientResponseHandler

commonErrorResponse :: CommonError -> Response
commonErrorResponse = match (Builder.build commonResponseHandler {})
