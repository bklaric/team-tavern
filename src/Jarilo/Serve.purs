module Jarilo.Serve where

import Prelude

import Async (Async, fromEffectCont, unify)
import Data.Either (Either(..), hush)
import Data.Maybe (fromJust)
import Data.MultiMap (MultiMap)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Effect (Effect)
import Jarilo.Junction (Junction, NamedRoute)
import Jarilo.Router.Junction (class JunctionRouter, router)
import Jarilo.Serve.SendResponse.Response (sendResponse)
import Node.Server (ListenOptions)
import Partial.Unsafe (unsafePartial)
import Perun.Async.Server (run_)
import Perun.Request as PerunReq
import Perun.Request.Body (Body, readAsUtf8)
import Perun.Response (notFound_, notFound__)
import Perun.Response as PerunRes
import Perun.Url (pathSegments, queryPairs)
import Prim.Row (class Nub)
import Type.Proxy (Proxy)

-- type Request methods =
--     { method :: Variant methods
--     , url :: Either String Url
--     , headers :: Map String String
--     , cookies :: Map String String
--     , body :: Body
--     }



-- class Serve (junction :: Junction) errors results responses | junction -> errors results responses where
--     serve
--         :: Proxy junction
--         -> ListenOptions
--         -> (Either (Record errors) (Variant results) -> (forall left. Async left (Variant responses)))
--         -> Unit

-- instance Serve (NamedRoute name route) errors results responses where
--     serve _ options handler =
--         run_ options \request -> do


-- readBody :: forall left. Body -> Async left String
-- readBody body = flip readAsUtf8 body >>> void # fromEffectCont

-- serve :: forall junction errors results response. JunctionRouter junction () errors results => Proxy junction -> ListenOptions -> (forall left. Either (Record errors) (Variant results) -> Async left response) -> Effect Unit
serve
    :: forall junction errors results
    .  JunctionRouter junction () errors results
    => Proxy junction
    -> ListenOptions
    -> (PerunReq.Request -> Record errors -> (forall left. Async left PerunRes.Response))
    -> (PerunReq.Request -> Variant results -> (forall left. Async left PerunRes.Response))
    -> Effect Unit
serve proxy options notFoundHandler handler =
    run_ options \request ->
        case router proxy request of
        Left errors -> notFoundHandler request errors
        Right result -> handler request result
        -- router
        -- body' <- readBody body
        -- response <- router proxy method (pathSegments (url # hush # fromJust)) (queryPairs (url # hush # fromJust)) body' # handler
        -- sendResponse response
