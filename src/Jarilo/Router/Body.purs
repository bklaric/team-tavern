module Jarilo.Router.Body where

import Prelude

import Async (Async)
import Data.Bifunctor (lmap)
import Jarilo.Types (Body, JsonBody, NoBody)
import Perun.Async.Request.Body (readBody)
import Perun.Request.Body as PerunReq
import Type.Proxy (Proxy)
import Yoga.JSON (class ReadForeign, class WriteForeign, writeJSON)
import Yoga.JSON.Async (readJSON)

data BodyError = CantParseJson String

class BodyRouter (body :: Body) realBody | body -> realBody where
    bodyRouter :: Proxy body -> PerunReq.Body -> Async BodyError realBody
    responseBodyRouter :: Proxy body -> realBody -> String

instance BodyRouter NoBody Unit where
    bodyRouter _ _ = pure unit
    responseBodyRouter _ _ = mempty

instance (ReadForeign realBody, WriteForeign realBody) => BodyRouter (JsonBody realBody) realBody where
    bodyRouter _ body = body # readBody >>= readJSON # lmap (show >>> CantParseJson)
    responseBodyRouter _ body = writeJSON body
