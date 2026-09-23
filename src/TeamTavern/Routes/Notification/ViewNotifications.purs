module TeamTavern.Routes.Notification.ViewNotifications where

import Data.Maybe (Maybe)
import Jarilo (type (!), type (==>), Get_, Internal_, Literal, NotAuthorized_, OkJson)

-- | The signed-in player's notifications (brief 11.3), newest first.
type ViewNotifications =
    Get_ (Literal "notifications")
    ==> OkJson OkContent ! NotAuthorized_ ! Internal_

-- | The player's own post a notification is about. `game` is the game's title,
-- | and `expires` the time the post runs out.
type NotificationPost =
    { id :: Int
    , type :: String
    , name :: Maybe String
    , owner :: String
    , handle :: String
    , game :: String
    , expires :: String
    }

-- | A post that fits the player's, in the same game.
type FittingPost =
    { id :: Int
    , type :: String
    , name :: Maybe String
    , owner :: String
    }

-- | A `fit` names the post that fits; an `expiry` is about the post alone.
-- | `created` is when it last fired.
type Notification =
    { id :: Int
    , kind :: String
    , created :: String
    , read :: Boolean
    , post :: NotificationPost
    , fitting :: Maybe FittingPost
    }

type OkContent = Array Notification
