module TeamTavern.Server.Team.Infrastructure.ValidateWebsite where

import Prelude

import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Data.Validated as Validated
import TeamTavern.Server.Infrastructure.Error (ValidatedTerrorNeaVar)
import TeamTavern.Server.Infrastructure.Error as Terror
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url, validateUrlV_)
import Type.Proxy (Proxy(..))

validateWebsite :: forall errors.
    Maybe String -> ValidatedTerrorNeaVar (website :: {} | errors) (Maybe Url)
validateWebsite website
    = website
    # traverse validateUrlV_
    # Validated.lmap
        ((\errors -> Terror.singleton {} ("Error validating website: " <> show errors))
        >>> Terror.labelNea (Proxy :: _ "website"))
