module TeamTavern.Server.Country.ViewCountries (loadCountries, viewCountries) where

import Prelude

import Async (Async)
import Data.Variant (Variant)
import Jarilo (InternalRow_, OkRow, ok_)
import JavaScript.Npm.Pg.Pool (Pool)
import JavaScript.Npm.Pg.Query (Query(..))
import TeamTavern.Routes.Country.ViewCountries as ViewCountries
import TeamTavern.Server.Infrastructure.Postgres (queryFirstInternal_)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Infrastructure.SendResponse (sendResponse)
import Type.Row (type (+))

loadCountriesQuery :: Query
loadCountriesQuery = Query """
    select
        array(select name from region order by ordinal) as regions,
        coalesce((
            select jsonb_agg(jsonb_build_object('name', country.name, 'region', region.name)
                order by country.name)
            from country
                join region on region.name = country.region_name
        ), '[]') as countries
    """

loadCountries :: ∀ errors. Pool -> Async (InternalTerror_ errors) ViewCountries.OkContent
loadCountries pool = queryFirstInternal_ pool loadCountriesQuery

viewCountries :: ∀ left. Pool -> Async left (Variant (OkRow ViewCountries.OkContent + InternalRow_ + ()))
viewCountries pool =
    sendResponse "Error viewing countries" do
    ok_ <$> loadCountries pool
