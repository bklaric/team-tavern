module Jarilo.Route where

import Jarilo.Method (Method)
import Jarilo.Path (Path)
import Jarilo.Query (Query)
import Jarilo.Response (Response)

foreign import data Route :: Type

foreign import data FullRoute :: Method -> Path -> Query -> Response -> Route
