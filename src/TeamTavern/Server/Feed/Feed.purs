module TeamTavern.Server.Feed.Feed (feedQuery) where

import JavaScript.Npm.Pg.Query (Query(..))

foreign import feedText :: String

-- | One batch of a game's feed, as `Feed.sql` describes it.
feedQuery :: Query
feedQuery = Query feedText
