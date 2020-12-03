module TeamTavern.Server.Team.Infrastructure.ValidateWebsite where

import Prelude

import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Validated.Label (VariantValidated)
import Data.Validated.Label as Validated
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url, UrlErrors, validateUrlV_)

validateWebsite :: forall errors.
    Maybe String -> VariantValidated (website :: Array String | errors) (Maybe Url)
validateWebsite website
    = website
    # traverse validateUrlV_
    # Validated.labelMap (SProxy :: SProxy "website") \(errors :: UrlErrors) ->
        [ "Error validating website: " <> show errors]
