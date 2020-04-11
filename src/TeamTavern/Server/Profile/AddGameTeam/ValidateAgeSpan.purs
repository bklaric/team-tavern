module TeamTavern.Server.Profile.AddGameTeam.ValidateAgeSpan
    (Age, AgeSpan, nullableAgeFrom, nullableAgeTo, validateAgeSpan) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)

newtype Age = Age Int

validateAge :: Int -> Maybe Age
validateAge age | age >= 13 = Just $ Age age
validateAge age = Nothing

data AgeSpan = AgeSpan (Maybe Age) (Maybe Age)

nullableAgeFrom :: AgeSpan -> Nullable Age
nullableAgeFrom (AgeSpan from _) = toNullable from

nullableAgeTo :: AgeSpan -> Nullable Age
nullableAgeTo (AgeSpan _ to) = toNullable to

validateAgeSpan :: Maybe Int -> Maybe Int -> AgeSpan
validateAgeSpan from to = AgeSpan (from >>= validateAge) (to >>= validateAge)
