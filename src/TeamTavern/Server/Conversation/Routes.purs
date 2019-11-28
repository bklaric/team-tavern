module TeamTavern.Server.Conversation.Routes where

import Jarilo.Junction (type (:<|>), type (:=))
import Jarilo.Method (Get, Post)
import Jarilo.Path (type (:>), End)
import Jarilo.Query (NoQuery)
import Jarilo.Route (Route)
import Jarilo.Segment (Capture, Literal)

type Nickname = String

type ViewAllConversations = Route
    Get
    (  Literal "conversations"
    :> End)
    NoQuery

type ViewConversation = Route
    Get
    (  Literal "conversations"
    :> Capture "nickname" Nickname
    :> End)
    NoQuery

type StartConversation = Route
    Post
    (  Literal "conversations"
    :> Capture "nickname" Nickname
    :> End)
    NoQuery

type ConversationRoutes
    =    "viewAllConversations" := ViewAllConversations
    :<|> "viewConversation"     := ViewConversation
    :<|> "startConversation"    := StartConversation
