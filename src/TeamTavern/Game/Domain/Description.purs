module TeamTavern.Game.Domain.Description
    (Description, DescriptionError, maxLength, create, create') where

import Prelude

import Data.Either (Either)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.Variant (Variant)
import Data.Validated (Validated, toEither)
import Wrapped.String (Empty, NotPrintable, TooLong, empty, notPrintable, tooLong)
import Wrapped.Validated as Wrapped

newtype Description = Description String

derive instance eqDescription :: Eq Description

derive instance newtypeDescription :: Newtype Description _

type DescriptionError = Variant
    ( empty :: Empty
    , tooLong :: TooLong
    , notPrintable :: NotPrintable
    )

maxLength :: Int
maxLength = 2000

create :: String -> Validated (NonEmptyList DescriptionError) Description
create description =
    Wrapped.create trim [empty, tooLong maxLength, notPrintable]
    Description description

create' :: String -> Either (NonEmptyList DescriptionError) Description
create' = create >>> toEither
