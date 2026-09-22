module TeamTavern.Routes.Feed.ViewOwnDescriptions where

import Data.Maybe (Maybe)
import Jarilo (type (!), type (/), type (==>), Capture, Get_, Internal_, Literal, NotAuthorized_, OkJson)
import TeamTavern.Routes.Shared.Description (Description)

-- | The signed-in player's posts in a game, each as the description it makes,
-- | so the feed can start from one and tell when it says what the post says.
type ViewOwnDescriptions =
    Get_ (Literal "games" / Capture "handle" String / Literal "own")
    ==> OkJson OkContent ! NotAuthorized_ ! Internal_

type OwnDescription =
    { type :: String
    , name :: Maybe String
    , description :: Description
    }

-- | Player post first, then group, then community.
type OkContent = Array OwnDescription
