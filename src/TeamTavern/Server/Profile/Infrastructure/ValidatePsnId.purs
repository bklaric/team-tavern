module TeamTavern.Server.Profile.Infrastructure.ValidatePsnId (PsnId, toString, validatePsnId) where

import Prelude

import Data.Maybe (Maybe)
import Data.String (length)
import Data.Validated.Label (ValidatedVariants)
import Data.Variant (SProxy(..))
import TeamTavern.Server.Profile.Infrastructure.ValidateContact (validateContact)

newtype PsnId = PsnId String

toString :: PsnId -> String
toString (PsnId psnId) = psnId

minNameLength :: Int
minNameLength = 3

maxNameLength :: Int
maxNameLength = 16

-- An Online ID is one's username on the PlayStation Network. It can range from 3 to 16 characters
-- in length and consist of letters, numbers, hyphens and underscores.
-- https://en.wikipedia.org/wiki/PlayStation_Network#Online_ID
isPsnIdValid :: String -> Boolean
isPsnIdValid psnId = minNameLength <= length psnId && length psnId <= maxNameLength

validatePsnId :: forall errors. Maybe String -> ValidatedVariants (psnId :: String | errors) (Maybe PsnId)
validatePsnId psnId =
    validateContact psnId isPsnIdValid PsnId (SProxy :: _ "psnId") ("Invalid PsnId: " <> _)
