module TeamTavern.Shared.Thin (thin) where

import Prelude

import Data.Array (filter, length)
import Data.String.Regex (split)
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)

-- | Whether a post says too little in its own words for its page to be worth
-- | a search engine's while. Its facts are the same as its card's in the feed,
-- | so they don't count. The page's robots tag and the sitemap both ask.
thin :: Array String -> Boolean
thin summary = summary >>= split (unsafeRegex "\\s+" unicode) # filter (_ /= "") # length # (_ < 20)
