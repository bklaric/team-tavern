module TeamTavern.Routes.Game.ViewGame where

import Jarilo (type (!), type (/), type (==>), Capture, Get_, Internal_, Literal, NotFound_, OkJson)
import TeamTavern.Routes.Shared.Field (Field)

type ViewGame =
    Get_ (Literal "games" / Capture "handle" String)
    ==> OkJson OkContent ! NotFound_ ! Internal_

-- | A tracker links a player's profile: its template followed by the player's
-- | account of the `contact` kind.
type Tracker =
    { contact :: String
    , title :: String
    , template :: String
    }

type OkContent =
    { handle :: String
    , title :: String
    , shortTitle :: String
    , active :: Int
    , contacts :: Array String
    , trackers :: Array Tracker
    , fields :: Array Field
    }
