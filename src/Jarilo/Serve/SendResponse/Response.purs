module Jarilo.Serve.SendResponse.Response where

import Prelude

import Async (Async)
import Data.MultiMap (MultiMap)
import Data.Variant (class VariantMatchCases, Variant, match)
import Jarilo.Response (type (:!), BadRequest, BadRequest_, Forbidden, Forbidden_, Internal, Internal_, NoContent, Ok, Ok_, Response, ResponseChain)
import Perun.Response as PerunRes
import Prim.Row (class Lacks, class Union)
import Prim.RowList (class RowToList)
import Record as Record
import Record.Builder (Builder, buildFromScratch)
import Record.Builder as Builder
import Type.Proxy (Proxy(..))
import Yoga.JSON (class WriteForeign, writeJSON)

class SendResponse (responseKind :: Response) responsesStartRow responsesEndRow | responseKind -> responsesStartRow responsesEndRow where
    sendResponse :: Proxy responseKind -> Builder (Record responsesStartRow) (Record responsesEndRow)

instance (WriteForeign body, Lacks "ok" responsesStartRow) =>
    SendResponse (Ok body) responsesStartRow (ok :: { body :: body, headers :: MultiMap String String } -> PerunRes.Response | responsesStartRow ) where
    sendResponse _ = Builder.insert (Proxy :: _ "ok")
        \({ headers, body }) -> { statusCode: 200, headers, body: writeJSON body }

instance (Lacks "ok" responsesStartRow) =>
    SendResponse Ok_ responsesStartRow (ok :: { headers :: MultiMap String String } -> PerunRes.Response | responsesStartRow ) where
    sendResponse _ = Builder.insert (Proxy :: _ "ok")
        \({ headers }) -> { statusCode: 200, headers, body: mempty}

instance (Lacks "noContent" responsesStartRow) =>
    SendResponse NoContent responsesStartRow (noContent :: { headers :: MultiMap String String } -> PerunRes.Response | responsesStartRow ) where
    sendResponse _ = Builder.insert (Proxy :: _ "noContent")
        \({ headers }) -> { statusCode: 204, headers, body: mempty}

instance (WriteForeign body, Lacks "badRequest" responsesStartRow) =>
    SendResponse (BadRequest body) responsesStartRow (badRequest :: { body :: body, headers :: MultiMap String String } -> PerunRes.Response | responsesStartRow ) where
    sendResponse _ = Builder.insert (Proxy :: _ "badRequest")
        \({ headers, body }) -> { statusCode: 400, headers, body: writeJSON body }

instance (Lacks "badRequest" responsesStartRow) =>
    SendResponse BadRequest_ responsesStartRow (badRequest :: { headers :: MultiMap String String } -> PerunRes.Response | responsesStartRow ) where
    sendResponse _ = Builder.insert (Proxy :: _ "badRequest")
        \({ headers }) -> { statusCode: 400, headers, body: mempty}

instance (WriteForeign body, Lacks "notAuthenticated" responsesStartRow) =>
    SendResponse (Forbidden body) responsesStartRow (notAuthenticated :: { body :: body, headers :: MultiMap String String } -> PerunRes.Response | responsesStartRow ) where
    sendResponse _ = Builder.insert (Proxy :: _ "notAuthenticated")
        \({ headers, body }) -> { statusCode: 401, headers, body: writeJSON body }

instance (Lacks "notAuthenticated" responsesStartRow) =>
    SendResponse Forbidden_ responsesStartRow (notAuthenticated :: { headers :: MultiMap String String } -> PerunRes.Response | responsesStartRow ) where
    sendResponse _ = Builder.insert (Proxy :: _ "notAuthenticated")
        \({ headers }) -> { statusCode: 401, headers, body: mempty}

instance (WriteForeign body, Lacks "internal" responsesStartRow) =>
    SendResponse (Internal body) responsesStartRow (internal :: { body :: body, headers :: MultiMap String String } -> PerunRes.Response | responsesStartRow ) where
    sendResponse _ = Builder.insert (Proxy :: _ "internal")
        \({ headers, body }) -> { statusCode: 500, headers, body: writeJSON body }

instance (Lacks "internal" responsesStartRow) =>
    SendResponse Internal_ responsesStartRow (internal :: { headers :: MultiMap String String } -> PerunRes.Response | responsesStartRow ) where
    sendResponse _ = Builder.insert (Proxy :: _ "internal")
        \({ headers }) -> { statusCode: 500, headers, body: mempty}

instance
    ( SendResponse leftResponseKind leftResponsesStartRow midResponsesRow
    , SendResponse rightResponseKind midResponsesRow rightResponsesEndRow
    ) =>
    SendResponse (leftResponseKind :! rightResponseKind) leftResponsesStartRow rightResponsesEndRow where
    sendResponse _ = sendResponse (Proxy :: _ leftResponseKind) >>> sendResponse (Proxy :: _ rightResponseKind)

sendResponse' :: forall perunResponse responseHandlerRowList wtf responseRow responseKind responseHandlerRow.
    RowToList responseHandlerRow responseHandlerRowList
    => VariantMatchCases responseHandlerRowList wtf perunResponse
    => Union wtf () responseRow
    => SendResponse responseKind () responseHandlerRow
    => Proxy responseKind
    -> Variant responseRow -> perunResponse
sendResponse' proxy response = response # match (sendResponse proxy # buildFromScratch)
