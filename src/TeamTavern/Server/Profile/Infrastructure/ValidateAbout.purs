module TeamTavern.Server.Profile.Infrastructure.ValidateAbout where

import Prelude

import Data.Array.NonEmpty as Nea
import Data.Validated as Validated
import Data.Variant (inj)
import TeamTavern.Server.Domain.Text (Text, validateText)
import TeamTavern.Server.Infrastructure.Error (Terror(..), ValidatedTerrorNeaVar)
import Type.Proxy (Proxy(..))

validateAbout :: forall errors.
    String -> ValidatedTerrorNeaVar (about :: {} | errors) Text
validateAbout about
    = validateText about
    # Validated.lmap \errors -> Terror
        (Nea.singleton $ inj (Proxy :: _ "about") {})
        [ "Error validating about text: " <> about
        , "Failed with following errors: " <> show errors
        ]
