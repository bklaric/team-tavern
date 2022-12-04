module TeamTavern.Server.Profile.Infrastructure.ValidateAmbitions where

import Prelude

import Data.Array.NonEmpty as Nea
import Data.Validated as Validated
import Data.Variant (inj)
import TeamTavern.Server.Domain.Text (Text, validateText)
import TeamTavern.Server.Infrastructure.Error (Terror(..), ValidatedTerrorNeaVar)
import Type.Proxy (Proxy(..))

validateAmbitions :: forall errors.
    String -> ValidatedTerrorNeaVar (ambitions :: {} | errors) Text
validateAmbitions ambitions
    = validateText ambitions
    # Validated.lmap \errors -> Terror
        (Nea.singleton $ inj (Proxy :: _ "ambitions") {})
        [ "Error validating ambitions text: " <> ambitions
        , "Failed with following errors: " <> show errors
        ]
