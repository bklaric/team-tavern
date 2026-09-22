module TeamTavern.Routes.All where

import Jarilo (type (<|>), type (:))
import TeamTavern.Routes.Country.ViewCountries (ViewCountries)
import TeamTavern.Routes.Feed.ViewFeed (ViewFeed)
import TeamTavern.Routes.Feed.ViewOwnDescriptions (ViewOwnDescriptions)
import TeamTavern.Routes.Game.ViewGame (ViewGame)
import TeamTavern.Routes.Game.ViewGames (ViewGames)
import TeamTavern.Routes.Password.ForgotPassword (ForgotPassword)
import TeamTavern.Routes.Password.ResetPassword (ResetPassword)
import TeamTavern.Routes.Player.ConfirmEmail (ConfirmEmail)
import TeamTavern.Routes.Player.RegisterPlayer (RegisterPlayer)
import TeamTavern.Routes.Player.ResendConfirmation (ResendConfirmation)
import TeamTavern.Routes.Player.ViewMe (ViewMe)
import TeamTavern.Routes.Session.EndSession (EndSession)
import TeamTavern.Routes.Session.StartSession (StartSession)

type SessionRoutes
    =   "startSession" : StartSession
    <|> "endSession"   : EndSession

type PasswordRoutes
    =   "forgotPassword" : ForgotPassword
    <|> "resetPassword"  : ResetPassword

type PlayerRoutes
    =   "registerPlayer"     : RegisterPlayer
    <|> "viewMe"             : ViewMe
    <|> "confirmEmail"       : ConfirmEmail
    <|> "resendConfirmation" : ResendConfirmation

type GameRoutes
    =   "viewGames" : ViewGames
    <|> "viewGame"  : ViewGame

type FeedRoutes
    =   "viewFeed"            : ViewFeed
    <|> "viewOwnDescriptions" : ViewOwnDescriptions

type CountryRoutes
    =   "viewCountries" : ViewCountries

type AllRoutes
    =    SessionRoutes
    <|> PasswordRoutes
    <|> PlayerRoutes
    <|> GameRoutes
    <|> FeedRoutes
    <|> CountryRoutes
