module Jarilo.Router.Junction where

import Prelude

import Async (Async, attempt, right)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant)
import Jarilo.Router.Request (RequestError)
import Jarilo.Router.Route (class RouteRouter, RouteResult(..), routeRouter)
import Jarilo.Types (Junction, JunctionChain, NamedRoute)
import Perun.Request as PerunReq
import Perun.Response as PerunRes
import Prim.Row (class Cons, class Union)
import Record (get, insert, union)
import Record as Record
import Type.Proxy (Proxy(..))

type RequestResult pathParams queryParams realBody =
    { endpoint ∷ String
    , path ∷ Record pathParams
    , query ∷ Record queryParams
    , headers ∷ Map String String
    , cookies ∷ Map String String
    , body ∷ realBody
    }

class JunctionRouter (junction ∷ Junction) errors handlers
    | junction -> errors handlers where
    junctionRouter
        ∷ Proxy junction
        -> Record handlers
        -> PerunReq.Request
        -> Async (Record errors) PerunRes.Response

instance
    ( RouteRouter route pathParams queryParams realBody responses
    , Cons name RequestError () errors
    , Cons name (RequestResult pathParams queryParams realBody -> Async (Record errors) (Variant responses)) handlers' handlers
    , IsSymbol name
    ) =>
    JunctionRouter (NamedRoute name route) errors handlers where
    junctionRouter _ handlers request = do
        RouteResult requestResult respond <- routeRouter (Proxy ∷ _ route) request
            # lmap (\error -> insert (Proxy ∷ _ name) error {})
        get (Proxy ∷ _ name) handlers (Record.insert (Proxy ∷ _ "endpoint") (reflectSymbol (Proxy ∷ _ name)) requestResult) <#> respond

instance
    ( JunctionRouter leftJunction leftErrors results
    , JunctionRouter rightJunction rightErrors results
    , Union leftErrors rightErrors errors
    ) =>
    JunctionRouter (JunctionChain leftJunction rightJunction) errors results where
    junctionRouter _ handlers request =
        junctionRouter (Proxy ∷ _ leftJunction) handlers request
        # attempt
        >>= case _ of
            Left leftBuilder ->
                junctionRouter (Proxy ∷ _ rightJunction) handlers request
                # lmap (union leftBuilder)
            Right leftRecord -> right leftRecord
