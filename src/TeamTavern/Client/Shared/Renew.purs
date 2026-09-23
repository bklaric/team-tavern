module TeamTavern.Client.Shared.Renew (renew, renewFailed) where

import Prelude

import Async (Async)
import Async as Async
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Variant (onMatch)
import TeamTavern.Client.Shared.Fetch (fetchPath)
import TeamTavern.Routes.Post.RenewPost (RenewPost)
import Type.Proxy (Proxy(..))

-- | Renews one of the player's posts in the game (brief 9), answering with what
-- | the toast then says, or nothing where it couldn't.
renew :: ∀ post left. String -> { id :: Int, type :: String | post } -> Async left (Maybe String)
renew handle post =
    Async.attempt (fetchPath (Proxy :: _ RenewPost) { handle, id: post.id }) <#> \result ->
        hush result >>= onMatch
            { noContent: const $ Just $ "Renewed. Your post stays active for "
                <> (if post.type == "community" then "90" else "30") <> " days from today."
            }
            (const Nothing)

renewFailed :: String
renewFailed = "Your post couldn't be renewed. Try again."
