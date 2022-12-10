module Data.Either.AlwaysRight where

import Prelude

import Data.Either (Either(..), either)

alwaysRight
    :: ∀ inLeft inRight outRight
    .  (inLeft -> outRight)
    -> (inRight -> outRight)
    -> Either inLeft inRight
    -> (∀ voidLeft. Either voidLeft outRight)
alwaysRight leftFunction rightFunction either' =
    Right $ either leftFunction rightFunction either'
