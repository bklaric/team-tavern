module TeamTavern.Architecture.Deployment where

import Data.Maybe (Maybe(..))
import Data.String (toLower)

data Deployment = Local | Cloud

fromString :: String -> Maybe Deployment
fromString string =
    case toLower string of
    "local" -> Just Local
    "cloud" -> Just Cloud
    _ -> Nothing
