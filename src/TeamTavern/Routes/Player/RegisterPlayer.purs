module TeamTavern.Routes.Player.RegisterPlayer where

import Data.Variant (Variant)
import Jarilo.Types (Post)
import Jarilo.Types (Literal)
import Jarilo.Types (NoQuery)
import Jarilo.Types (type (:!), BadRequest, Ok)
import Jarilo.Types (FullRoute)

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
