module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Effect.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "You should add some tests."
