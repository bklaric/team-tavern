module TeamTavern.Routes.Player.RegisterPlayer where

import Data.Variant (Variant)
import Jarilo.Method (Post)
import Jarilo.Path (Literal)
import Jarilo.Query (NoQuery)
import Jarilo.Response (type (:!), BadRequest, Ok)
import Jarilo.Route (FullRoute)

type RegisterPlayer = FullRoute
    (Post RequestContent)
    (Literal "players")
    NoQuery
    (Ok OkContent :! BadRequest BadContent)

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
