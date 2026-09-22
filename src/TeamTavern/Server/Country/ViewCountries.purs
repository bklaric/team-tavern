module TeamTavern.Server.Country.ViewCountries (viewCountries) where

import Prelude

import Async (Async)
import Jarilo (ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..))
import TeamTavern.Routes.Country.ViewCountries as ViewCountries
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal_)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)

loadCountriesQuery :: Query
loadCountriesQuery = Query """
    select
        array(select name from region order by ordinal) as regions,
        coalesce((
            select jsonb_agg(jsonb_build_object('name', country.name, 'region', region.name)
                order by region.ordinal, country.name)
            from country
                join region on region.name = country.region_name
        ), '[]') as countries
    """

loadCountries :: ∀ errors. Pool -> Async (InternalTerror_ errors) ViewCountries.OkContent
loadCountries pool = queryFirstInternal_ pool loadCountriesQuery

viewCountries :: ∀ left. Pool -> Async left _
viewCountries pool =
    sendResponse "Error viewing countries" do
    ok_ <$> loadCountries pool
