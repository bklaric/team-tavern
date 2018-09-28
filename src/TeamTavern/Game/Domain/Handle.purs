module TeamTavern.Game.Domain.Handle
    (Handle, HandleError, maxLength, create, create', create'') where

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
import Wrapped.String (Empty, NotAsciiAlphaNumUnderscore, TooLong, empty, notAsciiAlphaNumUnderscore, tooLong)
import Wrapped.Validated as Wrapped

newtype Handle = Handle String

derive instance eqHandle :: Eq Handle

derive instance newtypeHandle :: Newtype Handle _

derive instance genericHandle :: Generic Handle _

instance showHandle :: Show Handle where
    show = genericShow

type HandleError = Variant
    ( empty :: Empty
    , tooLong :: TooLong
    , notAsciiAlphaNumUnderscore :: NotAsciiAlphaNumUnderscore
    )

maxLength :: Int
maxLength = 50

create :: String -> Validated (NonEmptyList HandleError) Handle
create handle =
    Wrapped.create trim [empty, tooLong maxLength, notAsciiAlphaNumUnderscore]
    Handle handle

create' :: String -> Either (NonEmptyList HandleError) Handle
create' = create >>> toEither

create'' :: String -> Maybe Handle
create'' = create >>> Validated.hush
