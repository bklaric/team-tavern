module TeamTavern.Routes.All where

import Jarilo.Junction (type (:<|>), type (:=))
import TeamTavern.Routes.Session.EndSession (EndSession)
import TeamTavern.Routes.Session.StartSession (StartSession)

type SessionRoutes
    =    "startSession" := StartSession
    :<|> "endSession"   := EndSession

type AllRoutes = SessionRoutes
