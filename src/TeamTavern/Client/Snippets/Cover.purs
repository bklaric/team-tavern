module TeamTavern.Client.Snippets.Cover (coverPath, smallCoverPath, tileSources) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- | A game's cover as it is kept, 600x900, which a shared link shows.
coverPath :: String -> String
coverPath handle = "/images/games/" <> handle <> ".webp"

-- | The 400x600 copy `build-covers.mjs` makes of it, enough for a cover shown
-- | at 112px or less on any screen.
smallCoverPath :: String -> String
smallCoverPath handle = "/images/games/400/" <> handle <> ".webp"

-- | A cover grid's tile is a third of a phone, less the page's padding and the
-- | gaps (`CoverGrid.scss`), and 160px wider up. Only a wide phone at a density
-- | that needs more than the copy's 400px loads the original.
tileSources :: ∀ r i. String -> Array (HH.IProp (src :: String | r) i)
tileSources handle =
    [ HP.src $ smallCoverPath handle
    , HP.attr (HH.AttrName "srcset") $ smallCoverPath handle <> " 400w, " <> coverPath handle <> " 600w"
    , HP.attr (HH.AttrName "sizes") "(max-width: 639px) calc((100vw - 56px) / 3), 160px"
    ]
