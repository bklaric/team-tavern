module TeamTavern.Client.Script.Chips (ChipsFit, onChipsFit) where

import Prelude

import Effect (Effect)

-- | How many of the chips the description bar may keep behind More fit in its
-- | two rows beside the rest, and whether all of them do, leaving no More.
type ChipsFit = { shown :: Int, all :: Boolean }

-- | Calls back with how the description bar's chips fit, at most once a frame
-- | while the bar changes, until the returned effect stops it.
foreign import onChipsFit :: (ChipsFit -> Effect Unit) -> Effect (Effect Unit)
