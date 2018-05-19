module TeamTavern.Architecture where

import Prelude

import Control.Monad.Cont (ContT)
import Data.Validation.Semigroup (V)
import Effect (Effect)

type ContV invalid valid = ContT Unit Effect (V invalid valid)
