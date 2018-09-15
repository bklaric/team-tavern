module Async.Aff where

import Prelude

import Async (Async, runAsync)
import Data.Bifunctor (lmap)
import Effect.Aff (Aff, Error, makeAff)

asyncToAff
    :: forall left right
    .  (left -> Error)
    -> Async left right
    -> Aff right
asyncToAff toError async = async # lmap toError # flip runAsync # (\cont ->
    \callback -> cont callback <#> mempty) # makeAff
