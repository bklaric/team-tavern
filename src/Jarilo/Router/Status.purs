module Jarilo.Router.Status where

import Prelude

import Data.MultiMap (MultiMap)
import Jarilo.Types (BadRequest, Forbidden, Internal, NoContent, NotAuthorized, Ok, Status)
import Perun.Response as PerunRes
import Prim.Row (class Lacks)
import Record.Builder (Builder, buildFromScratch, insert)
import Type.Proxy (Proxy(..))

type ResponseConverter =
    { headers :: MultiMap String String, body :: String } -> PerunRes.Response

class StatusRouter (status :: Status) responsesStart responsesEnd | status -> responsesStart responsesEnd where
    statusRouter' :: Proxy status -> Builder (Record responsesStart) (Record responsesEnd)

instance (Lacks "ok" responsesStart) =>
    StatusRouter Ok responsesStart ("ok" :: ResponseConverter | responsesStart) where
    statusRouter' _ = insert (Proxy :: _ "ok")
        \({ headers, body }) -> { statusCode: 200, headers, body }

instance (Lacks "noContent" responsesStart) =>
    StatusRouter NoContent responsesStart ("noContent" :: ResponseConverter | responsesStart) where
    statusRouter' _ = insert (Proxy :: _ "noContent")
        \({ headers, body }) -> { statusCode: 204, headers, body }

instance (Lacks "badRequest" responsesStart) =>
    StatusRouter BadRequest responsesStart ("badRequest" :: ResponseConverter | responsesStart) where
    statusRouter' _ = insert (Proxy :: _ "badRequest")
        \({ headers, body }) -> { statusCode: 400, headers, body }

instance (Lacks "notAuthorized" responsesStart) =>
    StatusRouter NotAuthorized responsesStart ("notAuthorized" :: ResponseConverter | responsesStart) where
    statusRouter' _ = insert (Proxy :: _ "notAuthorized")
        \({ headers, body }) -> { statusCode: 401, headers, body }

instance (Lacks "forbidden" responsesStart) =>
    StatusRouter Forbidden responsesStart ("forbidden" :: ResponseConverter | responsesStart) where
    statusRouter' _ = insert (Proxy :: _ "forbidden")
        \({ headers, body }) -> { statusCode: 403, headers, body }

instance (Lacks "internal" responsesStart) =>
    StatusRouter Internal responsesStart ("internal" :: ResponseConverter | responsesStart) where
    statusRouter' _ = insert (Proxy :: _ "internal")
        \({ headers, body }) -> { statusCode: 500, headers, body }

statusRouter :: forall status responses. StatusRouter status () responses =>
    Proxy status -> Record responses
statusRouter proxy = statusRouter' proxy # buildFromScratch
