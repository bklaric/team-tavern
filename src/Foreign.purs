module Foreign where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either, hush)
import Data.Foreign (Foreign, ForeignError, readString)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)

readString' :: Foreign -> Either (NonEmptyList ForeignError) String
readString' = readString >>> runExcept

readStringMaybe :: Foreign -> Maybe String
readStringMaybe = readString' >>> hush
