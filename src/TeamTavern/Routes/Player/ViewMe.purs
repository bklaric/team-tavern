module TeamTavern.Routes.Player.ViewMe where

import Jarilo (type (!), type (==>), Get_, Internal_, Literal, NotAuthorized_, OkJson)

type ViewMe =
    Get_ (Literal "me")
    ==> (OkJson OkContent ! NotAuthorized_ ! Internal_)

-- | The games the player has posts in, with how many and of which types, in
-- | catalogue order.
type OkGameContent =
    { handle :: String
    , posts :: Int
    , types :: Array String
    }

type OkContent =
    { nickname :: String
    , unreadConversations :: Int
    , unreadNotifications :: Int
    , games :: Array OkGameContent
    }
