module TeamTavern.Server.Profile.AddTeamProfile.ValidateAmbitions where

import Prelude

import Data.Symbol (SProxy(..))
import Data.Validated.Label (VariantValidated)
import Data.Validated.Label as Validated
import TeamTavern.Server.Domain.Text (Text, validateText)

validateAmbitions :: forall errors.
    String -> VariantValidated (ambitions :: Array String | errors) Text
validateAmbitions about
    = validateText about
    # Validated.labelMap (SProxy :: SProxy "ambitions") \errors ->
        [ "Error validating ambitions text: " <> show errors ]
