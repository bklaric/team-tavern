module TeamTavern.Routes.All where

import Jarilo.Junction (type (:<|>), type (:=))
import TeamTavern.Routes.Session.EndSession (EndSession)
import TeamTavern.Routes.Session.StartSession (StartSession)
import TeamTavern.Routes.Team.CreateTeam (CreateTeam)
import TeamTavern.Routes.Team.UpdateTeam (UpdateTeam)
import TeamTavern.Routes.Team.UpdateTeamContacts (UpdateTeamContacts)
import TeamTavern.Routes.Team.ViewTeam (ViewTeam)

type SessionRoutes
    =    "startSession" := StartSession
    :<|> "endSession"   := EndSession

type TeamRoutes
    =    "viewTeam"   := ViewTeam
    :<|> "createTeam" := CreateTeam
    :<|> "updateTeam" := UpdateTeam
    :<|> "updateTeamContacts" := UpdateTeamContacts

type AllRoutes = SessionRoutes
