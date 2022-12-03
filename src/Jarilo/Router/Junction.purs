module Jarilo.Router.Junction where

import Prelude

import Async (Async, attempt, right)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Jarilo.Router.Request (RequestError, RequestResult)
import Jarilo.Router.Route (class RouteRouter, RouteResult(..), routeRouter)
import Jarilo.Types (Junction, JunctionChain, NamedRoute)
import Perun.Request as PerunReq
import Perun.Response as PerunRes
import Prim.Row (class Cons, class Lacks, class Union)
import Record (get, insert, union)
import Type.Proxy (Proxy(..))

class JunctionRouter (junction :: Junction) errors handlers
    | junction -> errors handlers where
    junctionRouter
        :: Proxy junction
        -> Record handlers
        -> PerunReq.Request
        -> Async (Record errors) PerunRes.Response

instance
    ( RouteRouter route pathParams queryParams realBody responses
    , Cons name RequestError () errors
    , Lacks name startErrors
    , IsSymbol name
    , Cons name (RequestResult pathParams queryParams realBody -> Variant responses) handlers' handlers
    ) =>
    JunctionRouter (NamedRoute name route) errors handlers where
    junctionRouter _ handlers request =
        routeRouter (Proxy :: _ route) request
        # bimap
            (\error -> insert (Proxy :: _ name) error {})
            \(RouteResult requestResult respond) ->
                get (Proxy :: _ name) handlers requestResult # respond

instance
    ( JunctionRouter leftJunction leftErrors results
    , JunctionRouter rightJunction rightErrors results
    , Union leftErrors rightErrors errors
    ) =>
    JunctionRouter (JunctionChain leftJunction rightJunction) errors results where
    junctionRouter _ handlers request =
        junctionRouter (Proxy :: _ leftJunction) handlers request
        # attempt
        >>= case _ of
            Left leftBuilder ->
                junctionRouter (Proxy :: _ rightJunction) handlers request
                # lmap (union leftBuilder)
            Right leftRecord -> right leftRecord
