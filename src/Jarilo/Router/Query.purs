module Jarilo.Router.Query where

import Prelude

import Data.Array (catMaybes)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple), snd)
import Data.Variant (Variant, inj)
import Jarilo.Shared.Component (class Component, fromComponent)
import Jarilo.Query (Mandatory, Many, NoQuery, Optional, Query, QueryChain, Rest)
import Jarilo.Shared.QueryPairs (delete, find, findAll)
import Prim.Row (class Cons, class Lacks)
import Record.Builder (Builder, insert)
import Type.Proxy (Proxy(..))
import URI.Extra.QueryPairs (Key, QueryPairs(..), Value, keyFromString, valueToString)

type QueryError = Variant
    ( missingParameter ::
        { parameterName :: String
        , query :: QueryPairs Key Value
        }
    , parameterParse ::
        { parameterName :: String
        , errorMessage :: String
        , actualValue :: Value
        }
    )

parameterParseError :: forall parameterName. IsSymbol parameterName =>
    Proxy parameterName -> Value -> String -> QueryError
parameterParseError nameProxy actualValue errorMessage =
    inj (Proxy :: _ "parameterParse")
    { parameterName: reflectSymbol nameProxy
    , errorMessage: errorMessage
    , actualValue: actualValue
    }

fromValue :: forall value. Component value => Value -> Either String value
fromValue = valueToString >>> fromComponent

fromValue' :: forall value. Component value =>
    Value -> Either { error :: String, value :: Value } value
fromValue' value = valueToString value # fromComponent # lmap { error: _, value }

class QueryRouter (query :: Query) (input :: Row Type) (output :: Row Type)
    | query -> input output where
    queryRouter
        :: Proxy query
        -> QueryPairs Key Value
        -> Either
            QueryError
            (Tuple
                (QueryPairs Key Value)
                (Builder (Record input) (Record output)))

instance QueryRouter NoQuery input input where
    queryRouter _ query = pure $ Tuple query identity

instance
    ( IsSymbol name
    , Component result
    , Lacks name input
    , Cons name (Maybe result) input output
    ) => QueryRouter (Optional name result) input output where
    queryRouter _ query = let
        nameProxy = (Proxy :: _ name)
        newQuery = delete (keyFromString $ reflectSymbol nameProxy) query
        in do
        Tuple newQuery <$>
            case find (keyFromString $ reflectSymbol nameProxy) query of
            Nothing -> Right $ insert nameProxy Nothing
            Just (Tuple _ Nothing) -> Right $ insert nameProxy Nothing
            Just (Tuple _ (Just value)) -> value # fromValue # bimap
                (parameterParseError nameProxy value)
                (Just >>> insert nameProxy)

instance
    ( IsSymbol name
    , Component result
    , Lacks name input
    , Cons name result input output
    ) => QueryRouter (Mandatory name result) input output where
    queryRouter _ query = let
        nameProxy = (Proxy :: _ name)
        newQuery = delete (keyFromString $ reflectSymbol nameProxy) query
        missingParameterError = Left
            $ inj (Proxy :: _ "missingParameter")
            { parameterName: reflectSymbol nameProxy, query: query }
        in
        Tuple newQuery <$>
            case find (keyFromString $ reflectSymbol nameProxy) query of
            Nothing -> missingParameterError
            Just (Tuple _ Nothing) -> missingParameterError
            Just (Tuple _ (Just value)) -> value # fromValue # bimap
                (parameterParseError nameProxy value)
                (insert nameProxy)

instance
    ( IsSymbol name
    , Component result
    , Lacks name input
    , Cons name (Array result) input output
    ) => QueryRouter (Many name result) input output where
    queryRouter _ query = let
        nameProxy = (Proxy :: _ name)
        newQuery = delete (keyFromString $ reflectSymbol nameProxy) query
        foundPairs = findAll (keyFromString $ reflectSymbol nameProxy) query
        foundValues = foundPairs <#> snd # catMaybes # traverse fromValue'
        in
        bimap
            (\{ error, value } -> parameterParseError nameProxy value error)
            (\foundValues' -> Tuple newQuery $ insert nameProxy foundValues')
            foundValues

instance
    ( QueryRouter leftQuery input midput
    , QueryRouter rightQuery midput output
    ) => QueryRouter (QueryChain leftQuery rightQuery) input output where
    queryRouter _ query = let
        leftQueryProxy = (Proxy :: _ leftQuery)
        rightQueryProxy = (Proxy :: _ rightQuery)
        in do
        Tuple leftQuery leftBuilder <- queryRouter leftQueryProxy query
        Tuple rightQuery rightBuilder <- queryRouter rightQueryProxy leftQuery
        pure $ Tuple rightQuery $ leftBuilder >>> rightBuilder

instance
    ( IsSymbol name
    , Lacks name input
    , Cons name (QueryPairs Key Value) input output
    ) => QueryRouter (Rest name) input output where
    queryRouter _ query =
        pure $ Tuple (QueryPairs []) (insert (Proxy :: _ name) query)

queryRouter' :: forall input output query. QueryRouter query input output =>
    Proxy query -> QueryPairs Key Value -> Either QueryError (Builder (Record input) (Record output))
queryRouter' proxy query = do
    Tuple _ builder <- queryRouter proxy query
    Right builder
