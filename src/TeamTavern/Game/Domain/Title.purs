module TeamTavern.Game.Domain.Title
    (Title(..), TitleError, maxLength, create) where

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

newtype Title = Title String

derive instance eqTitle :: Eq Title

derive instance newtypeTitle :: Newtype Title _

derive instance genericTitle :: Generic Title _

instance showTitle :: Show Title where
    show = genericShow

type TitleError = Variant
    ( empty :: Empty
    , tooLong :: TooLong
    , notPrintable :: NotPrintable
    )

maxLength :: Int
maxLength = 50

create :: String -> Validated (NonEmptyList TitleError) Title
create title =
    Wrapped.create trim [empty, tooLong maxLength, notPrintable] Title title

-- create' :: String -> Either (NonEmptyList TitleError) Title
-- create' = create >>> toEither

-- create'' :: String -> Maybe Title
-- create'' = create >>> Validated.hush
