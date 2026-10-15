module TeamTavern.Routes.Guide.ViewGuides where

import Jarilo (type (==>), Get_, Literal, OkJson)
import TeamTavern.Routes.Guide.ViewGuide (GuideGame)

type ViewGuides =
    Get_ (Literal "guides")
    ==> OkJson OkContent

-- | `updated` is an ISO date.
type OkGuideContent =
    { slug :: String
    , heading :: String
    , description :: String
    , updated :: String
    , games :: Array GuideGame
    }

-- | The guides last updated come first.
type OkContent = Array OkGuideContent
