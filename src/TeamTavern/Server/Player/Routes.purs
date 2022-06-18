module TeamTavern.Server.Player.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Post, Put)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (FullRoute)
import Jarilo.Segment (Capture, Literal)
import TeamTavern.Routes.DeletePlayer (DeletePlayer)
import TeamTavern.Routes.UpdatePlayerContacts (UpdatePlayerContacts)
import TeamTavern.Routes.ViewPlayer (ViewPlayer)
import TeamTavern.Server.Player.Domain.Nickname (Nickname)

type RegisterPlayer = FullRoute
    Post
    (  Literal "players"
    :> End)
    NoQuery

type UpdatePlayer = FullRoute
    Put
    (  Literal "players"
    :> Capture "nickname" Nickname
    :> End)
    NoQuery

type PlayerRoutes
    =    "viewPlayer"     := ViewPlayer
    :<|> "registerPlayer" := RegisterPlayer
    :<|> "updatePlayer"   := UpdatePlayer
    :<|> "deletePlayer"   := DeletePlayer
    :<|> "updatePlayerContacts" := UpdatePlayerContacts
