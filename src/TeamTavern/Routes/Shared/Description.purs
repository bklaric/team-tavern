module TeamTavern.Routes.Shared.Description where

import Data.Maybe (Maybe)
import Foreign.Object (Object)

-- | What a viewer says about themselves, or a group or a community about the
-- | players it wants (brief 7.1), in the shape `Server/Feed/Feed.sql` reads:
-- | the feed's description bar writes one, and a post makes one of itself.
-- |
-- | `options` holds each game field's chosen option keys, a player's point on
-- | an ordered field among them; `ranges` a group's or community's ends of an
-- | ordered field; `flags` the booleans said yes to. A player gives `country`
-- | and `age`, a group or a community the `regions` and ages it wants.
-- | `online` is in `timezone`, and either end may be missing while it is
-- | being filled in, which compares nothing.
type Description =
    { type :: String
    , options :: Object (Array String)
    , ranges :: Object Range
    , flags :: Array String
    , country :: Maybe String
    , age :: Maybe Int
    , regions :: Array String
    , ageFrom :: Maybe Int
    , ageTo :: Maybe Int
    , languages :: Array String
    , online :: Maybe Hours
    , timezone :: Maybe String
    , microphone :: Boolean
    }

type Range = { from :: Maybe String, to :: Maybe String }

-- | Whole hours, "19:00".
type Hours = { from :: Maybe String, to :: Maybe String }
