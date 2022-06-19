module TeamTavern.Routes.All where

import Jarilo.Junction (type (:<|>), type (:=))
import TeamTavern.Routes.Profile.AddPlayerProfile (AddPlayerProfile)
import TeamTavern.Routes.Profile.AddTeamProfile (AddTeamProfile)
import TeamTavern.Routes.Profile.UpdatePlayerProfile (UpdatePlayerProfile)
import TeamTavern.Routes.Profile.UpdateTeamProfile (UpdateTeamProfile)
import TeamTavern.Routes.Profile.ViewPlayerProfile (ViewPlayerProfile)
import TeamTavern.Routes.Profile.ViewPlayerProfilesByGame (ViewPlayerProfilesByGame)
import TeamTavern.Routes.Profile.ViewTeamProfile (ViewTeamProfile)
import TeamTavern.Routes.Profile.ViewTeamProfilesByGame (ViewTeamProfilesByGame)
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

type ProfileRoutes
    =    "addPlayerProfile"         := AddPlayerProfile
    :<|> "addTeamProfile"           := AddTeamProfile
    :<|> "updatePlayerProfile"      := UpdatePlayerProfile
    :<|> "updateTeamProfile"        := UpdateTeamProfile
    :<|> "viewPlayerProfilesByGame" := ViewPlayerProfilesByGame
    :<|> "viewTeamProfilesByGame"   := ViewTeamProfilesByGame
    :<|> "viewPlayerProfile"        := ViewPlayerProfile
    :<|> "viewTeamProfile"          := ViewTeamProfile

type AllRoutes = SessionRoutes :<|> TeamRoutes :<|> ProfileRoutes
