module TeamTavern.Server.Profile.Infrastructure.ValidateEaId
  ( EaId
  , toString
  , validateEaId
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Data.String (length)
import Data.Validated.Label (ValidatedVariants)
import Type.Proxy (Proxy(..))
import TeamTavern.Server.Profile.Infrastructure.ValidateContact (validateContact)

newtype EaId = EaId String

toString :: EaId -> String
toString (EaId eaId) = eaId

minIdLength :: Int
minIdLength = 4

isEaIdValid :: String -> Boolean
isEaIdValid eaId = minIdLength <= length eaId

validateEaId :: forall errors. Maybe String -> ValidatedVariants (eaId :: String | errors) (Maybe EaId)
validateEaId eaId =
    validateContact eaId isEaIdValid EaId (Proxy :: _ "eaId") ("Invalid EA ID: " <> _)
