module TeamTavern.Client.Shared.Fetch where

import Prelude

import Browser.Fetch (Credentials(..))
import Data.Maybe (Maybe(..))
import Jarilo as Jarilo

fetch proxy path query body = let
    options = Jarilo.defaultOptions { pathPrefix = Just "/api", credentials = Just Include }
    in
    Jarilo.fetch proxy path query body options

fetchBody proxy body = fetch proxy {} {} body

fetchPathBody proxy path body = fetch proxy path {} body

fetchSimple proxy = fetch proxy {} {} unit
