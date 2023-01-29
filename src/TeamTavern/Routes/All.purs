module TeamTavern.Routes.All where

import Jarilo (type (<|>), type (:))
import TeamTavern.Routes.Alert.CreateAlert (CreateAlert)
import TeamTavern.Routes.Alert.DeleteAlert (DeleteAlert)
import TeamTavern.Routes.Boarding.Onboard (Onboard)
import TeamTavern.Routes.Boarding.Preboard (Preboard)
import TeamTavern.Routes.Game.ViewAllGames (ViewAllGames)
import TeamTavern.Routes.Game.ViewGame (ViewGame)
import TeamTavern.Routes.Oauth.DiscordOauth (DiscordOauth)
import TeamTavern.Routes.Oauth.DiscordOauthExists (DiscordOauthExists)
import TeamTavern.Routes.Password.ForgotPassword (ForgotPassword)
import TeamTavern.Routes.Password.ResetPassword (ResetPassword)
import TeamTavern.Routes.Player.DeletePlayer (DeletePlayer)
import TeamTavern.Routes.Player.RegisterPlayer (RegisterPlayer)
import TeamTavern.Routes.Player.UpdatePlayer (UpdatePlayer)
import TeamTavern.Routes.Player.UpdatePlayerContacts (UpdatePlayerContacts)
import TeamTavern.Routes.Player.UpdatePlayerEmail (UpdatePlayerEmail)
import TeamTavern.Routes.Player.UpdatePlayerPassword (UpdatePlayerPassword)
import TeamTavern.Routes.Player.ViewPlayer (ViewPlayer)
import TeamTavern.Routes.Profile.AddPlayerProfile (AddPlayerProfile)
import TeamTavern.Routes.Profile.AddTeamProfile (AddTeamProfile)
import TeamTavern.Routes.Profile.DeletePlayerProfile (DeletePlayerProfile)
import TeamTavern.Routes.Profile.DeleteTeamProfile (DeleteTeamProfile)
import TeamTavern.Routes.Profile.UpdatePlayerProfile (UpdatePlayerProfile)
import TeamTavern.Routes.Profile.UpdateTeamProfile (UpdateTeamProfile)
import TeamTavern.Routes.Profile.ViewPlayerProfile (ViewPlayerProfile)
import TeamTavern.Routes.Profile.ViewPlayerProfilesByGame (ViewPlayerProfilesByGame)
import TeamTavern.Routes.Profile.ViewTeamProfile (ViewTeamProfile)
import TeamTavern.Routes.Profile.ViewTeamProfilesByGame (ViewTeamProfilesByGame)
import TeamTavern.Routes.Session.EndSession (EndSession)
import TeamTavern.Routes.Session.StartSession (StartSession)
import TeamTavern.Routes.Team.CreateTeam (CreateTeam)
import TeamTavern.Routes.Team.DeleteTeam (DeleteTeam)
import TeamTavern.Routes.Team.UpdateTeam (UpdateTeam)
import TeamTavern.Routes.Team.UpdateTeamContacts (UpdateTeamContacts)
import TeamTavern.Routes.Team.ViewTeam (ViewTeam)

type SessionRoutes
    =   "startSession" : StartSession
    <|> "endSession"   : EndSession

type PasswordRoutes
    =   "forgotPassword" : ForgotPassword
    <|> "resetPassword"  : ResetPassword

type GameRoutes
    =   "viewAllGames" : ViewAllGames
    <|> "viewGame"     : ViewGame

type PlayerRoutes
    =   "viewPlayer"     : ViewPlayer
    <|> "registerPlayer" : RegisterPlayer
    <|> "updatePlayer"   : UpdatePlayer
    <|> "deletePlayer"   : DeletePlayer
    <|> "updatePlayerContacts" : UpdatePlayerContacts
    <|> "updatePlayerEmail"    : UpdatePlayerEmail
    <|> "updatePlayerPassword" : UpdatePlayerPassword

type TeamRoutes
    =   "viewTeam"   : ViewTeam
    <|> "createTeam" : CreateTeam
    <|> "updateTeam" : UpdateTeam
    <|> "deleteTeam" : DeleteTeam
    <|> "updateTeamContacts" : UpdateTeamContacts

type ProfileRoutes
    =   "addPlayerProfile"         : AddPlayerProfile
    <|> "addTeamProfile"           : AddTeamProfile
    <|> "updatePlayerProfile"      : UpdatePlayerProfile
    <|> "updateTeamProfile"        : UpdateTeamProfile
    <|> "deletePlayerProfile"      : DeletePlayerProfile
    <|> "deleteTeamProfile"        : DeleteTeamProfile
    <|> "viewPlayerProfilesByGame" : ViewPlayerProfilesByGame
    <|> "viewTeamProfilesByGame"   : ViewTeamProfilesByGame
    <|> "viewPlayerProfile"        : ViewPlayerProfile
    <|> "viewTeamProfile"          : ViewTeamProfile

type BoardRoutes
    =   "onboard"  : Onboard
    <|> "preboard" : Preboard

type AlertRoutes
    =   "createAlert" : CreateAlert
    <|> "deleteAlert" : DeleteAlert

type OauthRoutes
    = "discordOauth"         : DiscordOauth
    <|> "discordOauthExists" : DiscordOauthExists

type AllRoutes
    =    SessionRoutes
    <|> PasswordRoutes
    <|> GameRoutes
    <|> PlayerRoutes
    <|> TeamRoutes
    <|> ProfileRoutes
    <|> BoardRoutes
    <|> AlertRoutes
    <|> OauthRoutes
