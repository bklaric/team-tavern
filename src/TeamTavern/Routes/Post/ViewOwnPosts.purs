module TeamTavern.Routes.Post.ViewOwnPosts where

import Jarilo (type (!), type (==>), Get_, Internal_, Literal, NotAuthorized_, OkJson)
import TeamTavern.Routes.Shared.Card (CardRow)
import TeamTavern.Routes.Shared.Description (Description)
import TeamTavern.Routes.Shared.OwnPost (OwnerView)

-- | The signed-in player's posts, for the home page (brief 11.2).
type ViewOwnPosts =
    Get_ (Literal "own")
    ==> OkJson OkContent ! NotAuthorized_ ! Internal_

-- | `description` is the one the post makes, which See what fits opens the
-- | feed with.
type OwnPost =
    { post :: CardRow
    , owner :: OwnerView
    , description :: Description
    }

-- | Games in the catalogue's order, so renewing a post doesn't move it, and a
-- | game's posts player first, then group, then community.
type OkContent = Array { handle :: String, posts :: Array OwnPost }
