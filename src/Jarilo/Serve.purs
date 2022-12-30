module Jarilo.Serve where

import Prelude

import Async (examineLeftWithEffect, unify)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.HTTP.Method (CustomMethod, Method)
import Data.List (List)
import Data.Map (Map)
import Effect (Effect)
import Jarilo.Router.Junction (class JunctionRouter, junctionRouter)
import Node.Server (ListenOptions)
import Perun.Async.Server (run_)
import Perun.Request (Request)
import Record.Extra (pick)
import TeamTavern.Server.Infrastructure.Log (logStamped)
import Type.Proxy (Proxy)
import URI.Extra.QueryPairs (Key, QueryPairs, Value)
import URI.Path.Segment (PathSegment)

type ShowableRequest =
    { method :: Either CustomMethod Method
    , path :: List PathSegment
    , query :: QueryPairs Key Value
    , cookies :: Map String String
    }

serve :: ∀ errors handlers junction. JunctionRouter junction errors handlers =>
    Proxy junction -> ListenOptions -> Record handlers -> Effect Unit
serve proxy options handlers =
    run_ options \request ->
        junctionRouter proxy handlers request
        # examineLeftWithEffect (const $ logStamped $ show $ (pick :: Request -> ShowableRequest) request)
        # lmap (const { statusCode: 404, headers: mempty, body: mempty })
        # unify
