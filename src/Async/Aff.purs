module Async.Aff where

import Prelude

import Async (Async(..), runAsync)
import Control.Monad.Cont (ContT(..))
import Control.Monad.Except (ExceptT(..))
import Data.Bifunctor (lmap)
import Effect.Aff (Aff, Error, makeAff, runAff_)
import Unsafe.Coerce (unsafeCoerce)

asyncToAff
    :: ∀ left right
    .  (left -> Error)
    -> Async left right
    -> Aff right
asyncToAff toError async = async # lmap toError # flip runAsync # (\cont ->
    \callback -> cont callback <#> mempty) # makeAff

affToAsync :: ∀ left right. Aff right -> Async left right
affToAsync aff = unsafeCoerce $ Async $ ExceptT $ ContT
    \cont -> runAff_ cont aff
