module TeamTavern.Server.Account.Infrastructure.ValidateAccount
    (AccountChecked, accountChecks, blank, normalizedAccount) where

import Prelude

import Data.Array (all, any, elem)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Nea
import Data.Date (Date, exactDate)
import Data.Enum (toEnum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), joinWith, split, trim)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Validated (Validated, invalid)
import Data.Variant (Variant, inj)
import Foreign.Object as Object
import TeamTavern.Routes.Country.ViewCountries as ViewCountries
import TeamTavern.Routes.Shared.Post (AccountContent)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Shared.Languages (allLanguages)
import TeamTavern.Shared.Timezones (allTimezones)
import Type.Proxy (Proxy(..))

-- | A check of the account's facts and contacts, naming a fact by its key and
-- | a contact by its kind.
type AccountChecked errors = Validated
    (Terror (NonEmptyArray (Variant (field :: { key :: String }, contact :: { kind :: String } | errors))))
    Unit

blank :: Maybe String -> Maybe String
blank value = value <#> trim >>= \value' -> if value' == "" then Nothing else Just value'

normalizedAccount :: AccountContent -> AccountContent
normalizedAccount account = account
    { country = blank account.country
    , birthday = blank account.birthday
    , timezone = blank account.timezone
    , contacts = account.contacts <#> trim # Object.filter (_ /= "")
    }

parseDate :: String -> Maybe Date
parseDate text = case split (Pattern "-") text <#> Int.fromString of
    [ Just year, Just month, Just day ] -> do
        year' <- toEnum year
        month' <- toEnum month
        day' <- toEnum day
        exactDate year' month' day'
    _ -> Nothing

-- | Checks a normalized account against the countries, languages and
-- | timezones there are, and its contacts against the kinds `contactKinds`
-- | allows.
accountChecks :: ∀ errors.
    Array String -> ViewCountries.OkContent -> Date -> AccountContent -> Array (AccountChecked errors)
accountChecks contactKinds countries today account =
    [ ensure (maybe true (\country -> any (_.name >>> eq country) countries.countries) account.country)
        (fieldError "location") ("Unknown country: " <> fromMaybe "" account.country)
    , ensure (all (flip elem allLanguages) account.languages) (fieldError "languages")
        ("Unknown languages: " <> joinWith ", " account.languages)
    , ensure (maybe true (\birthday -> parseDate birthday # maybe false (_ <= today)) account.birthday)
        (fieldError "birthday") ("Birthday isn't a past date: " <> fromMaybe "" account.birthday)
    , ensure (maybe true (\timezone -> any (_.name >>> eq timezone) allTimezones) account.timezone)
        (fieldError "timezone") ("Unknown timezone: " <> fromMaybe "" account.timezone)
    ]
    <> (Object.toUnfoldable account.contacts <#> \(Tuple kind value) ->
        ensure
            (elem kind contactKinds && String.length value <= (if kind == "discord" then 37 else 100))
            (inj (Proxy :: _ "contact") { kind })
            ("Contact isn't allowed or is too long: " <> kind))
    where
    ensure true _ _ = pure unit
    ensure false error line = invalid $ Terror (Nea.singleton error) [ line ]
    fieldError key = inj (Proxy :: _ "field") { key }
