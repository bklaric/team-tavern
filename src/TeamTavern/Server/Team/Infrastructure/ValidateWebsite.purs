module TeamTavern.Server.Team.Infrastructure.ValidateWebsite where

import Prelude

import Data.Maybe (Maybe)
import Type.Proxy (Proxy(..))
import Data.Traversable (traverse)
import Data.Validated.Label (ValidatedVariants)
import Data.Validated.Label as Validated
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url, UrlErrors, validateUrlV_)

validateWebsite :: forall errors.
    Maybe String -> ValidatedVariants (website :: Array String | errors) (Maybe Url)
validateWebsite website
    = website
    # traverse validateUrlV_
    # Validated.labelMap (Proxy :: _ "website") \(errors :: UrlErrors) ->
        [ "Error validating website: " <> show errors]
