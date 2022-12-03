module Jarilo.Serve where

import Prelude

import Async (unify)
import Data.Bifunctor (lmap)
import Effect (Effect)
import Jarilo.Router.Junction (class JunctionRouter, junctionRouter)
import Node.Server (ListenOptions)
import Perun.Async.Server (run_)
import Perun.Response (notFound__)
import Type.Proxy (Proxy)

serve :: forall errors handlers junction. JunctionRouter junction errors handlers =>
    Proxy junction -> ListenOptions -> Record handlers -> Effect Unit
serve proxy options handlers =
    run_ options \request ->
        junctionRouter proxy handlers request
        # lmap (const notFound__)
        # unify