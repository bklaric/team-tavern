module TeamTavern.Server.Guide.Guides (Guide, guides) where

import Prelude

import Data.Array (sortBy)
import Data.Ord (comparing)

foreign import joinAnEsportsTeamText :: String

foreign import makeAnEsportsTeamText :: String

foreign import marvelRivalsChampionshipTeamText :: String

foreign import rocketLeagueTournamentsText :: String

foreign import theFinalsRankedWithFriendsText :: String

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
      , updated: "2026-10-06"
      , html: markdownToHtml joinAnEsportsTeamText
      }
    , { slug: "make-an-esports-team"
      , heading: "How to make an esports team"
      , title: "How to make an esports team: a captain's guide for Valorant, League, CS2 and more"
      , description: "Starting an amateur team? How to pick your format, recruit for the roles you're missing, "
            <> "run tryouts, find scrims, and the roster rules each game's tournament enforces."
      , published: "2026-09-29"
      , updated: "2026-10-06"
      , html: markdownToHtml makeAnEsportsTeamText
      }
    , { slug: "marvel-rivals-championship-team"
      , heading: "How to build a Marvel Rivals Championship team"
      , title: "Marvel Rivals Championship: how to build a Faction and qualify"
      , description: "Every member at Platinum 3, one platform, a roster that locks at sign-up: how to recruit a Faction "
            <> "for the Marvel Rivals Championship and the way to Ignite."
      , published: "2026-09-30"
      , updated: "2026-09-30"
      , html: markdownToHtml marvelRivalsChampionshipTeamText
      }
    , { slug: "rocket-league-tournaments"
      , heading: "How Rocket League tournaments work, and how to find a teammate for them"
      , title: "Rocket League tournaments: how they work and who to play them with"
      , description: "Several brackets a day in every region, a party entered at its best player's rank, a duo in 3v3 "
            <> "kept within three ranks: how Rocket League's tournaments work, and how to find a partner for them."
      , published: "2026-10-04"
      , updated: "2026-10-04"
      , html: markdownToHtml rocketLeagueTournamentsText
      }
    , { slug: "the-finals-ranked-with-friends"
      , heading: "How to play ranked with friends in The Finals"
      , title: "The Finals ranked with friends: who can queue together, and how to find a trio"
      , description: "A duo more than 10,000 RS apart can't queue, a trio can, and a party plays at its best player's rank: "
            <> "how ranked in The Finals treats friends, and how to find a steady third."
      , published: "2026-10-06"
      , updated: "2026-10-06"
      , html: markdownToHtml theFinalsRankedWithFriendsText
      }
    ]
