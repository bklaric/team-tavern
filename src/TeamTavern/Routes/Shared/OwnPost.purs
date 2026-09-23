module TeamTavern.Routes.Shared.OwnPost where

import Data.Maybe (Maybe)

-- | What a post's owner reads under its facts (brief 11.2): when it expires,
-- | how many conversations it has and how many of them are unread, and how
-- | often its contacts were shown. `conversation` is the one the count opens,
-- | the latest unread, else the latest.
type OwnerView =
    { expires :: String
    , conversations :: Int
    , unread :: Int
    , conversation :: Maybe Int
    , reveals :: Int
    }
