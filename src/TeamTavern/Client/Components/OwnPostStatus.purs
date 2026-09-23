module TeamTavern.Client.Components.OwnPostStatus (ownPostStatus) where

import Prelude

import Data.Array (catMaybes)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Halogen.HTML as HH
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Script.Ago (ago, millisOf)
import TeamTavern.Client.Snippets.Class as HS

day :: Number
day = 24.0 * 60.0 * 60.0 * 1000.0

plural :: Int -> String -> String
plural count noun = show count <> " " <> noun <> if count == 1 then "" else "s"

-- | What a post's owner is told about it under its facts (brief 11.2): how long
-- | it stays active, or what expiry means now that it has, how many
-- | conversations it produced and how often its contacts were shown.
ownPostStatus :: ∀ w i.
    { now :: Instant, expires :: String, conversations :: Int, reveals :: Int } -> HH.HTML w i
ownPostStatus { now, expires, conversations, reveals } = let
    left = millisOf expires - unwrap (unInstant now)
    days = round $ left / day
    state
        | left <= 0.0 =
            HH.span [ HS.class_ "own-post-state" ]
            [ Icons.clock
            , HH.text $ "Expired " <> ago now expires
                <> ". It's listed under older posts, and match emails are paused."
            ]
        -- The last week says so in full, with its icon, so it doesn't rely on color.
        | days <= 7 =
            HH.span [ HS.class_ "own-post-state own-post-state-soon" ]
            [ Icons.circleAlert
            , HH.text if days == 0 then "Expires today" else "Expires in " <> plural days "day"
            ]
        | otherwise =
            HH.span [ HS.class_ "own-post-state" ]
            [ Icons.clock, HH.text $ "Active for " <> show days <> " more days" ]
    stat icon text = HH.span [ HS.class_ "own-post-stat" ] [ icon, HH.text text ]
    in
    HH.div [ HS.class_ "own-post-status" ] $ catMaybes
    [ Just state
    , Just $ stat Icons.messageCircle
        if conversations == 0 then "No conversations yet" else plural conversations "conversation"
    , if reveals == 0 then Nothing
        else Just $ stat Icons.eye $ "Contacts shown " <> plural reveals "time"
    ]
