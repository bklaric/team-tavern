module TeamTavern.Server.Routes where

import Jarilo.Junction (type (:<|>))
import TeamTavern.Server.Conversation.Routes (ConversationRoutes)
import TeamTavern.Server.Game.Routes (GameRoutes)
import TeamTavern.Server.Player.Routes (PlayerRoutes)
import TeamTavern.Server.Profile.Routes (ProfileRoutes)
import TeamTavern.Server.Session.Routes (SessionRoutes)

type TeamTavernRoutes
    =    PlayerRoutes
    :<|> SessionRoutes
    :<|> GameRoutes
    :<|> ProfileRoutes
    :<|> ConversationRoutes
