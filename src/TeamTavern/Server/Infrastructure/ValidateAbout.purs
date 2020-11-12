module TeamTavern.Server.Infrastructure.ValidateAbout where

import Prelude

import Data.Symbol (SProxy(..))
import Data.Validated.Label (VariantValidated)
import Data.Validated.Label as Validated
import TeamTavern.Server.Domain.Text (Text, TextErrors, validateText)

validateAbout :: forall errors. String -> VariantValidated (about :: Array String | errors) Text
validateAbout about
    = validateText about
    # Validated.labelMap (SProxy :: SProxy "about") \(errors :: TextErrors) ->
        [ "Error validating about text: " <> show errors ]
