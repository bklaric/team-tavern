module TeamTavern.Server.Profile.Infrastructure.ValidateAbout where

import Prelude

import Type.Proxy (Proxy(..))
import Data.Validated.Label (ValidatedVariants)
import Data.Validated.Label as Validated
import TeamTavern.Server.Domain.Text (Text, validateText)

validateAbout :: forall errors.
    String -> ValidatedVariants (about :: Array String | errors) Text
validateAbout about
    = validateText about
    # Validated.labelMap (Proxy :: _ "about") \errors ->
        [ "Error validating about text: " <> about
        , "Failed with following errors: " <> show errors
        ]
