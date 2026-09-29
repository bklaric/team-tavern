module TeamTavern.Routes.Feed.ViewFeed where

import Data.Maybe (Maybe)
import Jarilo (type (!), type (/), type (==>), Capture, Internal_, Literal, NotFound_, OkJson, PostJson_)
import TeamTavern.Routes.Shared.Card (CardRow)
import TeamTavern.Routes.Shared.Description (Description)

-- | A batch of a game's feed. A POST, since the description is a JSON body; a
-- | crawler's render asks with an empty one.
type ViewFeed =
    PostJson_ (Literal "games" / Capture "handle" String / Literal "feed") RequestContent
    ==> OkJson OkContent ! NotFound_ ! Internal_

-- | Where the last batch ended, passed back as it came for the next one.
type Cursor =
    { expired :: Boolean
    , misses :: Int
    , updated :: String
    , id :: Int
    , shown :: Int
    }

-- | `showing` is the post types a player asks for: Showing's segment. A group
-- | or a community is shown players whatever it asks.
type RequestContent =
    { description :: Description
    , showing :: Array String
    , cursor :: Maybe Cursor
    }

-- | The active posts each tier holds, whichever of them the batch reaches.
type Tiers =
    { fits :: Int
    , missingOne :: Int
    , missingMore :: Int
    }

type OkContent =
    { posts :: Array CardRow
    , tiers :: Tiers
    , more :: Boolean
    , cursor :: Maybe Cursor
    }
