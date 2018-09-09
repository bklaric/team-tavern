module TeamTavern.Game.Domain.Name
    (Name, NameError, maxLength, create, create', create'') where

import Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.Validated (Validated, toEither)
import Data.Validated as Validated
import Data.Variant (Variant)
import Wrapped.String (Empty, NotPrintable, TooLong, empty, notPrintable, tooLong)
import Wrapped.Validated as Wrapped

newtype Name = Name String

derive instance eqName :: Eq Name

derive instance newtypeName :: Newtype Name _

derive instance genericName :: Generic Name _

instance showName :: Show Name where
    show = genericShow

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

create'' :: String -> Maybe Name
create'' = create >>> Validated.hush
