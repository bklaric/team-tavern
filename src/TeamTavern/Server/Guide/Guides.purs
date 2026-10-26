module TeamTavern.Server.Guide.Guides (Guide, guides) where

import Prelude

import Data.Array (sortBy)
import TeamTavern.Routes.Guide.ViewGuide (GuideGame)

foreign import deadlockRankedWithFriendsText :: String

foreign import fncsTeamText :: String

foreign import joinAnEsportsTeamText :: String

foreign import makeAnEsportsTeamText :: String

foreign import marvelRivalsChampionshipTeamText :: String

foreign import marvelRivalsRankedWithFriendsText :: String

foreign import overwatchRankedWithFriendsText :: String

foreign import rainbowSixSiegeRankedWithFriendsText :: String

foreign import rocketLeagueTournamentsText :: String

foreign import theFinalsRankedWithFriendsText :: String

foreign import valorantRankedWithFriendsText :: String

foreign import markdownToHtml :: String -> String

-- | The slug is the guide's URL, so it never changes. `updated` is the date a
-- | fact in the guide last changed. The dates are ISO. A guide that names one
-- | game in `games` shows its cover in the guides list and on a shared link;
-- | any other guide shows the logo.
type Guide =
    { slug :: String
    , heading :: String
    , title :: String
    , description :: String
    , published :: String
    , updated :: String
    , games :: Array GuideGame
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
      , updated: "2026-10-26"
      , games: []
      , html: markdownToHtml joinAnEsportsTeamText
      }
    , { slug: "make-an-esports-team"
      , heading: "How to make an esports team"
      , title: "How to make an esports team: a captain's guide for Valorant, League, CS2 and more"
      , description: "Starting an amateur team? How to pick your format, recruit for the roles you're missing, "
            <> "run tryouts, find scrims, and the roster rules each game's tournament enforces."
      , published: "2026-09-29"
      , updated: "2026-10-26"
      , games: []
      , html: markdownToHtml makeAnEsportsTeamText
      }
    , { slug: "marvel-rivals-championship-team"
      , heading: "How to build a Marvel Rivals Championship team"
      , title: "Marvel Rivals Championship: how to build a Faction and qualify"
      , description: "Every member at Platinum 3, one platform, a roster that locks at sign-up: how to recruit a Faction "
            <> "for the Marvel Rivals Championship and the way to Ignite."
      , published: "2026-09-30"
      , updated: "2026-10-21"
      , games: [ { handle: "marvel-rivals", title: "Marvel Rivals" } ]
      , html: markdownToHtml marvelRivalsChampionshipTeamText
      }
    , { slug: "marvel-rivals-ranked-with-friends"
      , heading: "Marvel Rivals team size, party size and playing ranked with friends"
      , title: "Marvel Rivals ranked with friends: party size and what ranks can play together"
      , description: "Marvel Rivals team size is six. Ranked with friends: any party size but five up to "
            <> "Grandmaster, duos above, and what ranks can play together."
      , published: "2026-10-21"
      , updated: "2026-10-21"
      , games: [ { handle: "marvel-rivals", title: "Marvel Rivals" } ]
      , html: markdownToHtml marvelRivalsRankedWithFriendsText
      }
    , { slug: "rocket-league-tournaments"
      , heading: "Rocket League tournament times, rules and finding a partner"
      , title: "Rocket League tournament times, rules and finding a partner"
      , description: "Every region's daily brackets in your own time, how long a run takes, what you need to "
            <> "enter, and why a party plays at its best player's rank: Rocket League's tournaments, and how to "
            <> "find a partner for them."
      , published: "2026-10-04"
      , updated: "2026-10-15"
      , games: [ { handle: "rocket-league", title: "Rocket League" } ]
      , html: markdownToHtml rocketLeagueTournamentsText
      }
    , { slug: "the-finals-ranked-with-friends"
      , heading: "How to play ranked with friends in The Finals"
      , title: "The Finals ranked with friends: who can queue together, and how to find a trio"
      , description: "A duo more than 10,000 RS apart can't queue, a trio can, and a party plays at its best player's rank: "
            <> "how ranked in The Finals treats friends, and how to find a steady third."
      , published: "2026-10-06"
      , updated: "2026-10-06"
      , games: [ { handle: "the-finals", title: "The Finals" } ]
      , html: markdownToHtml theFinalsRankedWithFriendsText
      }
    , { slug: "deadlock-ranked-with-friends"
      , heading: "Deadlock team size, party size and playing ranked with friends"
      , title: "Deadlock team size, party size and who can duo in ranked"
      , description: "A Deadlock team is six players, and a party of up to six plays Standard, but ranked takes "
            <> "only solos and duos one rank apart. Who can queue together, and how to find a duo."
      , published: "2026-10-07"
      , updated: "2026-10-20"
      , games: [ { handle: "deadlock", title: "Deadlock" } ]
      , html: markdownToHtml deadlockRankedWithFriendsText
      }
    , { slug: "rainbow-six-siege-ranked-with-friends"
      , heading: "How to play ranked with friends in Rainbow Six Siege"
      , title: "R6 ranked with friends: what ranks can play together in Siege, and how to find a stack"
      , description: "Since Ranked 3.0, a squad must be within three full ranks, two with a Diamond or Champion: "
            <> "who can queue together in Rainbow Six Siege, and how to find a stack in range."
      , published: "2026-10-15"
      , updated: "2026-10-15"
      , games: [ { handle: "rainbow-six-siege", title: "Rainbow Six Siege" } ]
      , html: markdownToHtml rainbowSixSiegeRankedWithFriendsText
      }
    , { slug: "overwatch-ranked-with-friends"
      , heading: "Overwatch team size, party size and playing ranked with friends"
      , title: "Overwatch ranked with friends: wide groups, party size and what ranks can play together"
      , description: "Overwatch team size is five. Ranked with friends: narrow and wide groups, what ranks can "
            <> "play together, and why a wide four can't queue."
      , published: "2026-10-21"
      , updated: "2026-10-21"
      , games: [ { handle: "overwatch", title: "Overwatch" } ]
      , html: markdownToHtml overwatchRankedWithFriendsText
      }
    , { slug: "valorant-ranked-with-friends"
      , heading: "Valorant team size, party size and playing ranked with friends"
      , title: "Valorant ranked with friends: what ranks can play together, five-stacks and party size"
      , description: "Valorant team size is five. Ranked with friends: what ranks can duo or trio, why four can't "
            <> "queue, and what a five-stack of any ranks costs."
      , published: "2026-10-21"
      , updated: "2026-10-21"
      , games: [ { handle: "valorant", title: "Valorant" } ]
      , html: markdownToHtml valorantRankedWithFriendsText
      }
    , { slug: "fncs-team"
      , heading: "FNCS 2027 is Trios: how to find a trio and qualify"
      , title: "FNCS 2027 is Trios: how to find a trio, who's eligible and when it should start"
      , description: "FNCS goes back to Trios in 2027. Who's eligible, why a trio can't sub or swap a player "
            <> "in the middle of a Major, when the season should start, and how to find two teammates now."
      , published: "2026-10-26"
      , updated: "2026-10-26"
      , games: [ { handle: "fortnite", title: "Fortnite" } ]
      , html: markdownToHtml fncsTeamText
      }
    ]
