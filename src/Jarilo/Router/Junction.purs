module Jarilo.Router.Junction where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant, inj)
import Jarilo.Junction (Junction, JunctionChain, NamedRoute)
import Jarilo.Router.Route (class RouteRouter, RouteError, RouteResult, routeRouter)
import Perun.Request as PerunReq
import Prim.Row (class Cons, class Lacks)
import Record.Builder (Builder, build, buildFromScratch, insert)
import Type.Proxy (Proxy(..))

class JunctionRouter
    (junction :: Junction) (startErrors :: Row Type) (endErrors :: Row Type) (results :: Row Type)
    | junction -> startErrors endErrors results where
    junctionRouter
        :: Proxy junction
        -> PerunReq.Request
        -> Either
            (Builder (Record startErrors) (Record endErrors))
            (Variant results)

instance
    ( RouteRouter route pathParams queryParams body responses
    , Lacks name startErrors
    , Cons name RouteError startErrors endErrors
    , Cons name (RouteResult pathParams queryParams body responses) inputRecords results
    , IsSymbol name
    ) =>
    JunctionRouter (NamedRoute name route) startErrors endErrors results where
    junctionRouter _ request =
        case routeRouter (Proxy :: _ route) request of
        Left routeError -> Left $ insert (Proxy :: _ name) routeError
        Right resultTuple -> Right $ inj (Proxy :: _ name) resultTuple

instance
    ( JunctionRouter leftJunction startErrors midErrors results
    , JunctionRouter rightJunction midErrors endErrors results
    ) =>
    JunctionRouter (JunctionChain leftJunction rightJunction) startErrors endErrors results where
    junctionRouter _ request = let
        leftProxy = (Proxy :: _ leftJunction)
        rightProxy = (Proxy :: _ rightJunction)
        in
        case junctionRouter leftProxy request of
        Left leftBuilder ->
            case junctionRouter rightProxy request of
            Left rightBuilder -> Left $ leftBuilder >>> rightBuilder
            Right rightRecord -> Right rightRecord
        Right leftRecord -> Right leftRecord

router
    :: forall junction errors results
    .  JunctionRouter junction () errors results
    => Proxy junction
    -> PerunReq.Request
    -> Either (Record errors) (Variant results)
router junctionProxy request =
    junctionRouter junctionProxy request
    # lmap buildFromScratch
