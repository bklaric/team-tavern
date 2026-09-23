module TeamTavern.Routes.All where

import Jarilo (type (<|>), type (:))
import TeamTavern.Routes.Block.Block (Block)
import TeamTavern.Routes.Block.ReportConversation (ReportConversation)
import TeamTavern.Routes.Block.ReportPost (ReportPost)
import TeamTavern.Routes.Block.Unblock (Unblock)
import TeamTavern.Routes.Block.ViewBlocked (ViewBlocked)
import TeamTavern.Routes.Conversation.SendMessage (SendMessage)
import TeamTavern.Routes.Conversation.SendReply (SendReply)
import TeamTavern.Routes.Conversation.ViewConversation (ViewConversation)
import TeamTavern.Routes.Conversation.ViewInbox (ViewInbox)
import TeamTavern.Routes.Conversation.ViewPostConversation (ViewPostConversation)
import TeamTavern.Routes.Country.ViewCountries (ViewCountries)
import TeamTavern.Routes.Feed.ViewFeed (ViewFeed)
import TeamTavern.Routes.Feed.ViewOwnDescriptions (ViewOwnDescriptions)
import TeamTavern.Routes.Game.ViewGame (ViewGame)
import TeamTavern.Routes.Game.ViewGames (ViewGames)
import TeamTavern.Routes.Notification.ReadNotification (ReadNotification)
import TeamTavern.Routes.Notification.ReadNotifications (ReadNotifications)
import TeamTavern.Routes.Notification.ViewNotifications (ViewNotifications)
import TeamTavern.Routes.Password.ForgotPassword (ForgotPassword)
import TeamTavern.Routes.Password.ResetPassword (ResetPassword)
import TeamTavern.Routes.Player.ConfirmEmail (ConfirmEmail)
import TeamTavern.Routes.Player.RegisterPlayer (RegisterPlayer)
import TeamTavern.Routes.Player.ResendConfirmation (ResendConfirmation)
import TeamTavern.Routes.Player.ViewMe (ViewMe)
import TeamTavern.Routes.Post.CreatePost (CreatePost)
import TeamTavern.Routes.Post.DeletePost (DeletePost)
import TeamTavern.Routes.Post.RenewPost (RenewPost)
import TeamTavern.Routes.Post.RevealContacts (RevealContacts)
import TeamTavern.Routes.Post.UpdatePost (UpdatePost)
import TeamTavern.Routes.Post.ViewOwnPost (ViewOwnPost)
import TeamTavern.Routes.Post.ViewOwnPosts (ViewOwnPosts)
import TeamTavern.Routes.Post.ViewPost (ViewPost)
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

type PostRoutes
    =   "viewPost"       : ViewPost
    <|> "viewOwnPosts"   : ViewOwnPosts
    <|> "viewOwnPost"    : ViewOwnPost
    <|> "createPost"     : CreatePost
    <|> "updatePost"     : UpdatePost
    <|> "renewPost"      : RenewPost
    <|> "revealContacts" : RevealContacts
    <|> "deletePost"     : DeletePost

type ConversationRoutes
    =   "viewInbox"            : ViewInbox
    <|> "viewConversation"     : ViewConversation
    <|> "viewPostConversation" : ViewPostConversation
    <|> "sendMessage"          : SendMessage
    <|> "sendReply"            : SendReply

type BlockRoutes
    =   "block"              : Block
    <|> "unblock"            : Unblock
    <|> "viewBlocked"        : ViewBlocked
    <|> "reportPost"         : ReportPost
    <|> "reportConversation" : ReportConversation

type NotificationRoutes
    =   "viewNotifications" : ViewNotifications
    <|> "readNotifications" : ReadNotifications
    <|> "readNotification"  : ReadNotification

type CountryRoutes
    =   "viewCountries" : ViewCountries

type AllRoutes
    =    SessionRoutes
    <|> PasswordRoutes
    <|> PlayerRoutes
    <|> GameRoutes
    <|> FeedRoutes
    <|> PostRoutes
    <|> ConversationRoutes
    <|> BlockRoutes
    <|> NotificationRoutes
    <|> CountryRoutes
