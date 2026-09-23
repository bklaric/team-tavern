module TeamTavern.Server.Notification.Infrastructure.Visible (visibleNotification) where

import Prelude

import TeamTavern.Server.Block.Infrastructure.Blocked (blockedBetween)

-- | Whether a notification is shown to the owner of its post: SQL over a
-- | `notification` and its `post`. A block made after a post fitting was told
-- | hides that notification, as it hides the post (brief 10).
visibleNotification :: String
visibleNotification = """
    not exists (
        select from post fitting
        where fitting.id = notification.fitting_post_id
            and """ <> blockedBetween "post.player_id" "fitting.player_id" <> """
    )
    """
