module TeamTavern.Routes.Guide.ViewGuides where

import Jarilo (type (==>), Get_, Literal, OkJson)

type ViewGuides =
    Get_ (Literal "guides")
    ==> OkJson OkContent

-- | `updated` is an ISO date.
type OkGuideContent =
    { slug :: String
    , heading :: String
    , description :: String
    , updated :: String
    }

-- | The guides last updated come first.
type OkContent = Array OkGuideContent
