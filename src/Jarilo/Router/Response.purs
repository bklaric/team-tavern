module Jarilo.Router.Response where

import Prelude

import Data.MultiMap (MultiMap)
import Data.Symbol (class IsSymbol)
import Data.Variant (class VariantMatchCases, Variant, match)
import Jarilo.Router.Body (class BodyRouter, responseBodyRouter)
import Jarilo.Types (BadRequest, Forbidden, FullResponse, Internal, NoContent, NotAuthorized, Ok, Response, ResponseChain)
import Perun.Response as PerunRes
import Prim.Row (class Cons, class Lacks, class Union)
import Prim.RowList (class RowToList)
import Record.Builder (Builder, buildFromScratch, insert)
import Type.Proxy (Proxy(..))

type ResponseConverter realBody =
    { headers :: MultiMap String String, body :: realBody } -> PerunRes.Response

responseRouter''
    :: forall label responsesStart responsesEnd body realBody
    .  Cons label (ResponseConverter realBody) responsesStart responsesEnd
    => Lacks label responsesStart
    => IsSymbol label
    => BodyRouter body realBody
    => Proxy label
    -> Proxy body
    -> Int
    -> Builder (Record responsesStart) (Record responsesEnd)
responseRouter'' labelProxy bodyProxy statusCode = insert labelProxy
    \({headers, body}) -> { statusCode, headers, body: responseBodyRouter bodyProxy body }

class ResponseRouter (response :: Response) responsesStart responsesEnd | response -> responsesStart responsesEnd where
    responseRouter'
        :: Proxy response
        -> Builder (Record responsesStart) (Record responsesEnd)

instance (Lacks "ok" responsesStart, BodyRouter body realBody) =>
    ResponseRouter (FullResponse Ok body) responsesStart (ok :: ResponseConverter realBody | responsesStart) where
    responseRouter' _ = responseRouter'' (Proxy :: _ "ok") (Proxy :: _ body) 200

instance (Lacks "noContent" responsesStart, BodyRouter body realBody) =>
    ResponseRouter (FullResponse NoContent body) responsesStart (noContent :: ResponseConverter realBody | responsesStart) where
    responseRouter' _ = responseRouter'' (Proxy :: _ "noContent") (Proxy :: _ body) 204

instance (Lacks "badRequest" responsesStart, BodyRouter body realBody) =>
    ResponseRouter (FullResponse BadRequest body) responsesStart (badRequest :: ResponseConverter realBody | responsesStart) where
    responseRouter' _ = responseRouter'' (Proxy :: _ "badRequest") (Proxy :: _ body) 400

instance (Lacks "notAuthorized" responsesStart, BodyRouter body realBody) =>
    ResponseRouter (FullResponse NotAuthorized body) responsesStart (notAuthorized :: ResponseConverter realBody | responsesStart) where
    responseRouter' _ = responseRouter'' (Proxy :: _ "notAuthorized") (Proxy :: _ body) 401

instance (Lacks "forbidden" responsesStart, BodyRouter body realBody) =>
    ResponseRouter (FullResponse Forbidden body) responsesStart (forbidden :: ResponseConverter realBody | responsesStart) where
    responseRouter' _ = responseRouter'' (Proxy :: _ "forbidden") (Proxy :: _ body) 403

instance (Lacks "internal" responsesStart, BodyRouter body realBody) =>
    ResponseRouter (FullResponse Internal body) responsesStart (internal :: ResponseConverter realBody | responsesStart) where
    responseRouter' _ = responseRouter'' (Proxy :: _ "internal") (Proxy :: _ body) 500

instance (ResponseRouter leftResponse responsesStart responsesMid, ResponseRouter rightResponse responsesMid responsesEnd) =>
    ResponseRouter (ResponseChain leftResponse rightResponse) responsesStart responsesEnd where
    responseRouter' _ = responseRouter' (Proxy :: _ leftResponse) >>> responseRouter' (Proxy :: _ rightResponse)

responseRouter
    :: forall responseHandlerRowList wtf responseRow response responseHandlerRow
    .  RowToList responseHandlerRow responseHandlerRowList
    => VariantMatchCases responseHandlerRowList wtf PerunRes.Response
    => Union wtf () responseRow
    => ResponseRouter response () responseHandlerRow
    => Proxy response
    -> Variant responseRow -> PerunRes.Response
responseRouter proxy response = response # match (responseRouter' proxy # buildFromScratch)
