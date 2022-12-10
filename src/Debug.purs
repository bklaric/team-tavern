module Debug where

import Prelude

import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Unsafe.Coerce (unsafeCoerce)

pipeLog :: ∀ a. a -> a
pipeLog a = unsafePerformEffect (log (unsafeCoerce a) $> a)
