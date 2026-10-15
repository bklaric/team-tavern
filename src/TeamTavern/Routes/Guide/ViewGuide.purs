module TeamTavern.Routes.Guide.ViewGuide where

import Jarilo (type (!), type (/), type (==>), Capture, Get_, Literal, NotFound_, OkJson)

type ViewGuide =
    Get_ (Literal "guides" / Capture "slug" String)
    ==> OkJson OkContent ! NotFound_

-- | A game the guide is about, whose cover stands for it.
type GuideGame = { handle :: String, title :: String }

-- | `title` names the guide in search results, and `heading` heads its page.
-- | The dates are ISO, and `html` is the guide's text.
type OkContent =
    { heading :: String
    , title :: String
    , description :: String
    , published :: String
    , updated :: String
    , games :: Array GuideGame
    , html :: String
    }
