module TeamTavern.Client.Script.Share (canShare, shareAsync) where

import Prelude

import Async (Async, fromEitherCont)
import Data.Either (Either(..))
import Effect (Effect)
import JavaScript.Error (Error)

foreign import canShare :: Effect Boolean

foreign import shareImpl
    :: (Error -> Effect Unit)
    -> Effect Unit
    -> { title :: String, url :: String }
    -> Effect Unit

shareAsync :: { title :: String, url :: String } -> Async Error Unit
shareAsync shared = fromEitherCont \callback -> shareImpl (Left >>> callback) (callback $ Right unit) shared
