module TeamTavern.Server.Profile.Infrastructure.ValidateAmbitions where

import Prelude

import Type.Proxy (Proxy(..))
import Data.Validated.Label (ValidatedVariants)
import Data.Validated.Label as Validated
import TeamTavern.Server.Domain.Text (Text, validateText)

validateAmbitions :: forall errors.
    String -> ValidatedVariants (ambitions :: Array String | errors) Text
validateAmbitions ambitions
    = validateText ambitions
    # Validated.labelMap (Proxy :: _ "ambitions") \errors ->
        [ "Error validating ambitions text: " <> ambitions
        , "Failed with following errors: " <> show errors
        ]
