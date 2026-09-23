module TeamTavern.Routes.Shared.OwnPost where

-- | What a post's owner reads under its facts (brief 11.2): when it expires,
-- | how many conversations it has and how many of them are unread, and how
-- | often its contacts were shown.
type OwnerView =
    { expires :: String
    , conversations :: Int
    , unread :: Int
    , reveals :: Int
    }
