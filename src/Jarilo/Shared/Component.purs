module Jarilo.Shared.Component where

import Prelude

import Data.Either (Either(..), note)
import Data.Int as Int
import Data.String (toLower)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty as NonEmptyString

class Component value where
    fromComponent :: String -> Either String value
    toComponent :: value -> String

instance Component String where
    fromComponent = pure
    toComponent = identity


instance Component Int where
    fromComponent =
        Int.fromString >>> note "Couldn't turn component into an integer."
    toComponent = show

instance Component Boolean where
    fromComponent = toLower >>>
        case _ of
        "true" -> Right true
        "false" -> Right false
        _ -> Left "Couldn't turn component into a boolean."
    toComponent = show

instance Component NonEmptyString where
    fromComponent = NonEmptyString.fromString >>> note "Component cannot be empty."
    toComponent = NonEmptyString.toString
