module TeamTavern.Routes.Post.ViewOwnPost where

import Data.Maybe (Maybe)
import Jarilo (type (!), type (/), type (==>), Capture, Get_, Internal_, Literal, NotAuthorized_, OkJson)
import TeamTavern.Routes.Shared.Post (AccountContent, PostContent)

-- | The signed-in player's post of a type in a game, if they have one, and
-- | what their account holds, which the post screen shows and prefills from.
type ViewOwnPost =
    Get_ (Literal "games" / Capture "handle" String / Literal "own" / Capture "type" String)
    ==> OkJson OkContent ! NotAuthorized_ ! Internal_

-- | `conversations` is how many the post has, which deleting it deletes.
type OwnPost =
    { id :: Int
    , updated :: String
    , conversations :: Int
    , content :: PostContent
    }

type OkContent =
    { account :: AccountContent
    , post :: Maybe OwnPost
    }
