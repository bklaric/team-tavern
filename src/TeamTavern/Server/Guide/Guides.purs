module TeamTavern.Server.Guide.Guides (Guide, guides) where

import Prelude

import Data.Array (sortBy)
import Data.Ord (comparing)

foreign import joinAnEsportsTeamText :: String

foreign import makeAnEsportsTeamText :: String

foreign import markdownToHtml :: String -> String

-- | The slug is the guide's URL, so it never changes. `updated` is the date a
-- | fact in the guide last changed. The dates are ISO.
type Guide =
    { slug :: String
    , heading :: String
    , title :: String
    , description :: String
    , published :: String
    , updated :: String
    , html :: String
    }

-- | The guides last updated come first. Each is turned into HTML once, as the
-- | server starts.
guides :: Array Guide
guides = sortBy (flip $ comparing _.updated)
    [ { slug: "join-an-esports-team"
      , heading: "How to join an esports team"
      , title: "How to join an esports team: the way in for Valorant, League, CS2 and more"
      , description: "Most esports teams are amateur five-stacks playing Premier, Clash, ESEA or Siege Cup. "
            <> "Here's what each game asks of your account, what teams look for, and where to find one."
      , published: "2026-09-29"
      , updated: "2026-09-29"
      , html: markdownToHtml joinAnEsportsTeamText
      }
    , { slug: "make-an-esports-team"
      , heading: "How to make an esports team"
      , title: "How to make an esports team: a captain's guide for Valorant, League, CS2 and more"
      , description: "Starting an amateur team? How to pick your format, recruit for the roles you're missing, "
            <> "run tryouts, find scrims, and the roster rules each game's tournament enforces."
      , published: "2026-09-29"
      , updated: "2026-09-29"
      , html: markdownToHtml makeAnEsportsTeamText
      }
    ]
