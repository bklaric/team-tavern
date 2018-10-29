module TeamTavern.Profile.Domain.Summary
    (Summary, SummaryError, maxLength, create, create', create'') where

import Prelude

import Data.Either (Either)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.Validated (Validated, toEither)
import Data.Validated as Validated
import Data.Variant (Variant)
import Wrapped.String (NotPrintable, TooLong, Empty, empty, notPrintable, tooLong)
import Wrapped.Validated as Wrapped

newtype Summary = Summary String

derive instance newtypeSummary :: Newtype Summary _

type SummaryError = Variant
    ( empty :: Empty
    , tooLong :: TooLong
    , notPrintable :: NotPrintable
    )

maxLength :: Int
maxLength = 2000

create :: String -> Validated (NonEmptyList SummaryError) Summary
create summary =
    Wrapped.create trim [empty, tooLong maxLength, notPrintable]
        Summary summary

create' :: String -> Either (NonEmptyList SummaryError) Summary
create' = create >>> toEither

create'' :: String -> Maybe Summary
create'' = create >>> Validated.hush
