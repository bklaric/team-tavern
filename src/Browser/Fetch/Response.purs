module Browser.Fetch.Response
    ( Response
    , status
    , text
    , json
    ) where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Error (Error)
import Foreign (Foreign)

foreign import data Response :: Type

foreign import status :: Response -> Int

foreign import textImpl
    :: (String -> Effect Unit)
    -> Response
    -> Effect Unit

text :: forall left.
    (Either left String -> Effect Unit) -> Response -> Effect Unit
text callback = textImpl (Right >>> callback)

foreign import jsonImpl
    :: (Foreign -> Effect Unit)
    -> (Error -> Effect Unit)
    -> Response
    -> Effect Unit

json :: (Either Error Foreign -> Effect Unit) -> Response -> Effect Unit
json callback = jsonImpl (Right >>> callback) (Left >>> callback)
