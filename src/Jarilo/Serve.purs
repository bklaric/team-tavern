module Jarilo.Serve where

import Prelude

import Async (unify)
import Data.Bifunctor (lmap)
import Effect (Effect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Jarilo.Router.Junction (class JunctionRouter, junctionRouter)
import Node.Server (ListenOptions)
import Perun.Async.Server (run_)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

serve :: ∀ errors handlers junction. JunctionRouter junction errors handlers =>
    Proxy junction -> ListenOptions -> Record handlers -> Effect Unit
serve proxy options handlers =
    run_ options \request ->
        junctionRouter proxy handlers request
        # lmap (unsafeCoerce >>> log >>> unsafePerformEffect >>> const ({ statusCode: 404, headers: mempty, body: mempty }))
        # unify
