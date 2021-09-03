module TeamTavern.Server.Profile.Infrastructure.ValidateAbout where

import Prelude

import Data.Symbol (SProxy(..))
import Data.Validated.Label (ValidatedVariants)
import Data.Validated.Label as Validated
import TeamTavern.Server.Domain.Text (Text, validateText)

validateAbout :: forall errors.
    String -> ValidatedVariants (about :: Array String | errors) Text
validateAbout about
    = validateText about
    # Validated.labelMap (SProxy :: SProxy "about") \errors ->
        [ "Error validating about text: " <> about
        , "Failed with following errors: " <> show errors
        ]
