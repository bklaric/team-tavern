module TeamTavern.Routes.Session.StartSession where

import Data.Variant (Variant)
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Literal)

type StartSession = FullRoute
    Post
    (  Literal "sessions"
    :> End)
    NoQuery

type RequestContent =
    { nickname :: String
    , password :: String
    }

type BadContent = Variant (noSessionStarted :: {})
