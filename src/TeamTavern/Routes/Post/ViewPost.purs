module TeamTavern.Routes.Post.ViewPost where

import Data.Maybe (Maybe)
import Jarilo (type (!), type (/), type (==>), Capture, Get_, Internal_, Literal, NotFound_, OkJson)
import TeamTavern.Routes.Shared.Card (CardRow)

-- | A post's own page (brief 11.1). Not found is a post deleted, or one that
-- | isn't the game's.
type ViewPost =
    Get_ (Literal "games" / Capture "handle" String / Literal "posts" / Capture "id" Int)
    ==> OkJson OkContent ! NotFound_ ! Internal_

-- | What the post's owner reads under its facts: when it expires, how many
-- | conversations it has and how often its contacts were shown.
type OwnerView =
    { expires :: String
    , conversations :: Int
    , reveals :: Int
    }

-- | `blocked` is who blocked whom, `viewer` or `owner`, where either did: the
-- | page shows the post without its contact button (brief 10). `owner` is
-- | there only for the post's owner.
type OkContent =
    { post :: CardRow
    , blocked :: Maybe String
    , owner :: Maybe OwnerView
    }
