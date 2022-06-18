module TeamTavern.Routes.Player.RegisterPlayer where

import Data.Variant (Variant)
import Jarilo.Method (Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Literal)

type RegisterPlayer = FullRoute
    Post
    (  Literal "players"
    :> End)
    NoQuery

type RequestContent =
    { nickname :: String
    , password :: String
    }

type OkContent = { nickname :: String }

type BadContentIdentifiers = Variant
    ( invalidNickname :: {}
    , invalidPassword :: {}
    )

type BadContent = Variant
    ( registration :: Array BadContentIdentifiers
    , nicknameTaken :: {}
    )
