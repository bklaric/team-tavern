module Browser.Async.Fetch.Response
    ( module Browser.Fetch.Response
    , text
    , json
    ) where

import Prelude

import Async (Async, fromEitherCont)
import Browser.Fetch.Response (Response, status)
import Browser.Fetch.Response as FRes
import Error (Error)
import Foreign (Foreign)

text :: forall left. Response -> Async left String
text = flip FRes.text >>> fromEitherCont

json :: Response -> Async Error Foreign
json = flip FRes.json >>> fromEitherCont
