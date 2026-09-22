module TeamTavern.Routes.All where

import Jarilo (type (<|>), type (:))
import TeamTavern.Routes.Game.ViewGames (ViewGames)
import TeamTavern.Routes.Password.ForgotPassword (ForgotPassword)
import TeamTavern.Routes.Password.ResetPassword (ResetPassword)
import TeamTavern.Routes.Session.EndSession (EndSession)
import TeamTavern.Routes.Session.StartSession (StartSession)

type SessionRoutes
    =   "startSession" : StartSession
    <|> "endSession"   : EndSession

type PasswordRoutes
    =   "forgotPassword" : ForgotPassword
    <|> "resetPassword"  : ResetPassword

type GameRoutes
    =   "viewGames" : ViewGames

type AllRoutes
    =    SessionRoutes
    <|> PasswordRoutes
    <|> GameRoutes
