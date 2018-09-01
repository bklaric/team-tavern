module TeamTavern.Game.Domain.Handle
    (Handle, HandleError, maxLength, create, create') where

import Prelude

import Data.Either (Either)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.Variant (Variant)
import Data.Validated (Validated, toEither)
import Wrapped.String (Empty, NotAsciiAlphaNumUnderscore, TooLong, empty, notAsciiAlphaNumUnderscore, tooLong)
import Wrapped.Validated as Wrapped

newtype Handle = Handle String

derive instance eqHandle :: Eq Handle

derive instance newtypeHandle :: Newtype Handle _

type HandleError = Variant
    ( empty :: Empty
    , tooLong :: TooLong
    , notAsciiAlphaNumUnderscore :: NotAsciiAlphaNumUnderscore
    )

maxLength :: Int
maxLength = 50

create :: String -> Validated (NonEmptyList HandleError) Handle
create name =
    Wrapped.create trim [empty, tooLong maxLength, notAsciiAlphaNumUnderscore]
    Handle name

create' :: String -> Either (NonEmptyList HandleError) Handle
create' = create >>> toEither
