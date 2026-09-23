module TeamTavern.Server.Block.Infrastructure.Blocked (blockedBetween) where

import Prelude

-- | Whether either of two players, SQL expressions for their ids, has blocked
-- | the other: SQL that is true or false. A block hides both from each other
-- | (brief 10), so it is read both ways.
blockedBetween :: String -> String -> String
blockedBetween one other = """
    exists (
        select from block
        where blocker_id = """ <> one <> """ and blocked_id = """ <> other <> """
            or blocker_id = """ <> other <> """ and blocked_id = """ <> one <> """
    )
    """
