module TeamTavern.Routes.Country.ViewCountries where

import Jarilo (type (!), type (==>), Get_, Internal_, Literal, OkJson)

type ViewCountries =
    Get_ (Literal "countries")
    ==> OkJson OkContent ! Internal_

type Country =
    { name :: String
    , region :: String
    }

-- | The regions in their order, and the countries by region, then by name.
type OkContent =
    { regions :: Array String
    , countries :: Array Country
    }
