module TeamTavern.Client.Shared.Censor (censor) where

import Prelude

import Data.Array (replicate)
import Data.String (joinWith)
import Data.String.CodeUnits as CodeUnits
import Data.String.Regex (Regex, replace')
import Data.String.Regex.Flags (global, ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)

-- Slurs, each with the endings it takes. A post is shown with them starred
-- out rather than turned away, so a word the list catches by mistake costs a
-- few stars and not the post. The post keeps the words it was written with.
slurs :: Array String
slurs =
    [ "retard(?:s|ed)?"
    , "fag(?:s|got|gots)?"
    , "nigg(?:er|a|ah|uh)[sz]?"
    , "kikes?"
    , "spics?"
    , "chinks?"
    , "gooks?"
    , "coons?"
    , "wetbacks?"
    , "beaners?"
    , "pakis?"
    , "ragheads?"
    , "towelheads?"
    , "trann(?:y|ies)"
    , "shemales?"
    , "dykes?"
    ]

-- Whole words only, so a word that merely contains one, such as "retardant"
-- or "raccoon", stays as it is.
slursRegex :: Regex
slursRegex = unsafeRegex ("\\b(?:" <> joinWith "|" slurs <> ")\\b") (global <> ignoreCase)

-- | The text with every slur in it replaced by as many stars.
censor :: String -> String
censor = replace' slursRegex \match _ -> CodeUnits.fromCharArray $ replicate (CodeUnits.length match) '*'
