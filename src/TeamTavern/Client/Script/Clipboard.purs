module TeamTavern.Client.Script.Clipboard (writeText, writeTextAsync) where

import Prelude

import Async (Async, fromEitherCont)
import Data.Either (Either(..))
import Effect (Effect)
import Error (Error)

foreign import writeTextImpl
    :: (Error -> Effect Unit)
    -> Effect Unit
    -> String
    -> Effect Unit

writeText :: (Either Error Unit -> Effect Unit) -> String -> Effect Unit
writeText callback text =
    writeTextImpl (Left >>> callback) (callback $ Right unit) text

writeTextAsync :: String -> Async Error Unit
writeTextAsync text = fromEitherCont $ (flip writeText) text
