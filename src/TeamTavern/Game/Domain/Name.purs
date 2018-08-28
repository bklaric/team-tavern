module TeamTavern.Game.Domain.Name
    (Name, NameError, maxLength, create, create') where

import Prelude

import Data.Either (Either)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.Variant (Variant)
import Validated (Validated, toEither)
import Wrapped.String (Empty, NotPrintable, TooLong, empty, notPrintable, tooLong)
import Wrapped.Validated as Wrapped

newtype Name = Name String

derive instance eqName :: Eq Name

derive instance newtypeName :: Newtype Name _

type NameError = Variant
    ( empty :: Empty
    , tooLong :: TooLong
    , notPrintable :: NotPrintable
    )

maxLength :: Int
maxLength = 50

create :: String -> Validated (NonEmptyList NameError) Name
create name =
    Wrapped.create trim [empty, tooLong maxLength, notPrintable] Name name

create' :: String -> Either (NonEmptyList NameError) Name
create' = create >>> toEither
