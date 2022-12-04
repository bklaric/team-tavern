module TeamTavern.Routes.Team.UpdateTeam where

import Jarilo (type (!), type (/), type (==>), BadRequestJson, Capture, Literal, OkJson, PutJson_)
import TeamTavern.Routes.Team.CreateTeam as CreateTeam

type UpdateTeam =
    PutJson_
    ( Literal "teams"
    / Capture "handle" String)
    RequestContent
    ==> OkJson OkContent ! BadRequestJson BadContent

type RequestContent = CreateTeam.RequestContent

type OkContent = { handle :: String }

type BadContent = CreateTeam.BadContent
