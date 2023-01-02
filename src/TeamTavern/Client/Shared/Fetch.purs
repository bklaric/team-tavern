module TeamTavern.Client.Shared.Fetch where

import Prelude

import Async (attempt)
import Browser.Fetch (Credentials(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Variant (match, onMatch)
import Jarilo as Jarilo

fetch proxy path query body = let
    options = Jarilo.defaultOptions
        { pathPrefix = Just "/api", credentials = Just Include }
    in
    Jarilo.fetch proxy path query body options

fetchPath proxy path = fetch proxy path {} unit

fetchQuery proxy query = fetch proxy {} query unit

fetchBody proxy body = fetch proxy {} {} body

fetchPathQuery proxy path query = fetch proxy path query unit

fetchPathNoContent proxy path = fetchPath proxy path # attempt <#>
    case _ of
    Left _ -> Nothing
    Right response ->
        onMatch { noContent: const $ Just unit } (const Nothing) response

fetchPathBody proxy path body = fetch proxy path {} body

fetchSimple proxy = fetch proxy {} {} unit
