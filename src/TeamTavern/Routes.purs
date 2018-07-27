module TeamTavern.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import TeamTavern.Player.Routes (ViewPlayer, ViewPlayers, RegisterPlayer)
import TeamTavern.Player.Session.Routes (PrepareSession, StartSession)

type TeamTavernRoutes
    =    "viewPlayers"    := ViewPlayers
    :<|> "viewPlayer"     := ViewPlayer
    :<|> "registerPlayer" := RegisterPlayer
    :<|> "prepareSession" := PrepareSession
    :<|> "startSession"   := StartSession
