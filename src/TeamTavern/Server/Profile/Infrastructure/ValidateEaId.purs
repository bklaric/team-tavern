module TeamTavern.Server.Profile.Infrastructure.ValidateEaId (EaId, toString, validateEaId) where

import Prelude

import Data.Maybe (Maybe)
import Data.String (length)
import TeamTavern.Server.Infrastructure.Error (ValidatedTavern)
import TeamTavern.Server.Profile.Infrastructure.ValidateContact (validateContact)
import Type.Proxy (Proxy(..))

newtype EaId = EaId String

toString :: EaId -> String
toString (EaId eaId) = eaId

minIdLength :: Int
minIdLength = 4

isEaIdValid :: String -> Boolean
isEaIdValid eaId = minIdLength <= length eaId

validateEaId :: forall errors. Maybe String -> ValidatedTavern (eaId :: {} | errors) (Maybe EaId)
validateEaId eaId =
    validateContact eaId isEaIdValid EaId (Proxy :: _ "eaId") ("Invalid EA ID: " <> _)
