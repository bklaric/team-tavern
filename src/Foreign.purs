module Foreign where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Foreign (Foreign, readArray, readString)
import Data.Foreign.Class (class Decode, decode)
import Data.Foreign.Index (readProp)
import Data.Maybe (Maybe)

decode' :: forall value. Decode value => Foreign -> Maybe value
decode' = decode >>> runExcept >>> hush

readString' :: Foreign -> Maybe String
readString' = readString >>> runExcept >>> hush

readArray' :: Foreign -> Maybe (Array Foreign)
readArray' = readArray >>> runExcept >>> hush

readProp' :: String -> Foreign -> Maybe Foreign
readProp' property = readProp property >>> runExcept >>> hush
