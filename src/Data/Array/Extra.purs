module Data.Array.Extra where

import Prelude

import Data.Array as Array

full :: forall element. Array element -> Boolean
full = not <<< Array.null
