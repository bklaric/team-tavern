module TeamTavern.Client.Components.Card.Hours (Hours, inViewerTime, hoursText) where

import Prelude

import Data.Array (mapMaybe)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), split)
import TeamTavern.Client.Script.Clock (clock)

-- | Online hours in minutes after midnight. Equal ends are all day.
type Hours = { from :: Int, to :: Int }

-- | How many minutes a timezone is ahead of UTC at a moment.
foreign import offsetOf :: Number -> String -> Int

toMinutes :: String -> Maybe Int
toMinutes time = case mapMaybe fromString $ split (Pattern ":") time of
    [ hours, minutes ] -> Just $ hours * 60 + minutes
    _ -> Nothing

wrap :: Int -> Int
wrap minutes = ((minutes `mod` 1440) + 1440) `mod` 1440

-- | A post's hours, kept in its owner's timezone, as the viewer's clock reads
-- | them.
inViewerTime :: { now :: Instant, timezone :: String } -> Maybe String -> String -> String -> Maybe Hours
inViewerTime viewer ownerTimezone from to = do
    from' <- toMinutes from
    to' <- toMinutes to
    let millis = unwrap $ unInstant viewer.now
        shift = offsetOf millis viewer.timezone - offsetOf millis (case ownerTimezone of
            Just timezone -> timezone
            Nothing -> viewer.timezone)
    pure { from: wrap $ from' + shift, to: wrap $ to' + shift }

hoursText :: Hours -> String
hoursText { from, to } = clock from <> "–" <> clock to
