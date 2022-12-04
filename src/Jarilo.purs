module Jarilo (module Jarilo, module TypesExport, module Jarilo.Fetch, module Jarilo.Serve) where

import Jarilo.Fetch
import Jarilo.Serve
import Prelude

import Data.MultiMap (MultiMap)
import Data.Variant (Variant, inj)
import Jarilo.Router.Response (AppResponse(..))
import Jarilo.Types as Types
import Jarilo.Types hiding (Method, Options, Head, Get, Post, Put, Patch, Delete, Ok, NoContent, BadRequest, NotAuthorized, Forbidden, Internal) as TypesExport
import Type.Proxy (Proxy(..))

-------------------------------------------------
-- These shortcuts are for assembling route types
-------------------------------------------------

-- General request shortcuts

type Options path query = Types.FullRequest Types.Options path query Types.NoBody

type Head path query = Types.FullRequest Types.Head path query Types.NoBody

type Get path query = Types.FullRequest Types.Get path query Types.NoBody

type Post path query body = Types.FullRequest Types.Post path query body

type Put path query body = Types.FullRequest Types.Put path query body

type Patch path query body = Types.FullRequest Types.Patch path query body

type Delete path query = Types.FullRequest Types.Delete path query Types.NoBody

type Options_ path = Types.FullRequest Types.Options path Types.NoQuery Types.NoBody

type Head_ path = Types.FullRequest Types.Head path Types.NoQuery Types.NoBody

type Get_ path = Types.FullRequest Types.Get path Types.NoQuery Types.NoBody

type Post_ path body = Types.FullRequest Types.Post path Types.NoQuery body

type Put_ path body = Types.FullRequest Types.Put path Types.NoQuery body

type Patch_ path body = Types.FullRequest Types.Patch path Types.NoQuery body

type Delete_ path = Types.FullRequest Types.Delete path Types.NoQuery Types.NoBody

-- Json request shortcuts

type PostJson path query body = Types.FullRequest Types.Post path query (Types.JsonBody body)

type PutJson path query body = Types.FullRequest Types.Put path query (Types.JsonBody body)

type PatchJson path query body = Types.FullRequest Types.Patch path query (Types.JsonBody body)

type PostJson_ path body = Types.FullRequest Types.Post path Types.NoQuery (Types.JsonBody body)

type PutJson_ path body = Types.FullRequest Types.Put path Types.NoQuery (Types.JsonBody body)

type PatchJson_ path body = Types.FullRequest Types.Patch path Types.NoQuery (Types.JsonBody body)

-- General response shortcuts

type Ok body = Types.FullResponse Types.Ok body

type NoContent = Types.FullResponse Types.NoContent Types.NoBody

type BadRequest body = Types.FullResponse Types.BadRequest body

type NotAuthorized body = Types.FullResponse Types.NotAuthorized body

type Forbidden body = Types.FullResponse Types.Forbidden body

type Internal body = Types.FullResponse Types.Internal body

-- No body response shortcuts

type Ok_ = Types.FullResponse Types.Ok Types.NoBody

type BadRequest_ = Types.FullResponse Types.BadRequest Types.NoBody

type NotAuthorized_ = Types.FullResponse Types.NotAuthorized Types.NoBody

type Forbidden_ = Types.FullResponse Types.Forbidden Types.NoBody

type Internal_ = Types.FullResponse Types.Internal Types.NoBody

-- Json response shortcuts

type OkJson body = Types.FullResponse Types.Ok (Types.JsonBody body)

type BadRequestJson body = Types.FullResponse Types.BadRequest (Types.JsonBody body)

type NotAuthorizedJson body = Types.FullResponse Types.NotAuthorized (Types.JsonBody body)

type ForbiddenJson body = Types.FullResponse Types.Forbidden (Types.JsonBody body)

type InternalJson body = Types.FullResponse Types.Internal (Types.JsonBody body)

------------------------------------------------------------
-- These shortcuts are for returning responses from your app
------------------------------------------------------------

-- Concrete response rows

type OkRow body responses = (ok :: AppResponse body | responses)

type NoContentRow body responses = (noContent :: AppResponse body | responses)

type BadRequestRow body responses = (badRequest :: AppResponse body | responses)

type NotFoundRow body responses = (notFound :: AppResponse body | responses)

type NotAuthorizedRow body responses = (notAuthorized :: AppResponse body | responses)

type ForbiddenRow body responses = (forbidden :: AppResponse body | responses)

type InternalRow body responses = (internal :: AppResponse body | responses)

-- Concrete no body response rows

type OkRow_ responses = OkRow Unit responses

type NoContentRow_ responses = NoContentRow Unit responses

type BadRequestRow_ responses = BadRequestRow Unit responses

type NotFoundRow_ responses = NotFoundRow Unit responses

type NotAuthorizedRow_ responses = NotAuthorizedRow Unit responses

type ForbiddenRow_ responses = ForbiddenRow Unit responses

type InternalRow_ responses = InternalRow Unit responses

-- Concrete response shortcuts

ok :: forall responses body. MultiMap String String -> body -> Variant (OkRow body responses)
ok headers body = inj (Proxy :: _ "ok") $ AppResponse headers body

ok_ :: forall responses body. body -> Variant (OkRow body responses)
ok_ body = ok mempty body

ok__ :: forall responses. Variant (OkRow_ responses)
ok__ = ok_ unit

noContent :: forall responses. MultiMap String String -> Variant (NoContentRow_ responses)
noContent headers = inj (Proxy :: _ "noContent") $ AppResponse headers unit

noContent_ :: forall responses. Variant (NoContentRow_ responses)
noContent_ = noContent mempty

badRequest :: forall responses body. MultiMap String String -> body -> Variant (BadRequestRow body responses)
badRequest headers body = inj (Proxy :: _ "badRequest") $ AppResponse headers body

badRequest_ :: forall responses body. body -> Variant (BadRequestRow body responses)
badRequest_ body = badRequest mempty body

badRequest__ :: forall responses. Variant (BadRequestRow_ responses)
badRequest__ = badRequest_ unit

notAuthorized :: forall responses body. MultiMap String String -> body -> Variant (NotAuthorizedRow body responses)
notAuthorized headers body = inj (Proxy :: _ "notAuthorized") $ AppResponse headers body

notAuthorized_ :: forall responses body. body -> Variant (NotAuthorizedRow body responses)
notAuthorized_ body = notAuthorized mempty body

notAuthorized__ :: forall responses. Variant (NotAuthorizedRow_ responses)
notAuthorized__ = notAuthorized_ unit

forbidden :: forall responses body. MultiMap String String -> body -> Variant (ForbiddenRow body responses)
forbidden headers body = inj (Proxy :: _ "forbidden") $ AppResponse headers body

forbidden_ :: forall responses body. body -> Variant (ForbiddenRow body responses)
forbidden_ body = forbidden mempty body

forbidden__ :: forall responses. Variant (ForbiddenRow_ responses)
forbidden__ = forbidden_ unit

notFound :: forall responses body. MultiMap String String -> body -> Variant (NotFoundRow body responses)
notFound headers body = inj (Proxy :: _ "notFound") $ AppResponse headers body

notFound_ :: forall responses body. body -> Variant (NotFoundRow body responses)
notFound_ body = notFound mempty body

notFound__ :: forall responses. Variant (NotFoundRow_ responses)
notFound__ = notFound_ unit

internal :: forall responses body. MultiMap String String -> body -> Variant (InternalRow body responses)
internal headers body = inj (Proxy :: _ "internal") $ AppResponse headers body

internal_ :: forall responses body. body -> Variant (InternalRow body responses)
internal_ body = internal mempty body

internal__ :: forall responses. Variant (InternalRow_ responses)
internal__ = internal_ unit
