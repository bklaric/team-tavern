module TeamTavern.Player.Domain.Nickname
    ( Nickname
    , NicknameError
    , maxLength
    , create
    , create'
    , fromNonEmpty
    , fromNonEmpty'
    ) where

import Prelude

import Data.Either (Either)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.String.NonEmpty (NonEmptyString, toString)
import Data.Variant (Variant)
import Data.Validated (Validated, toEither)
import Wrapped.String (Empty, NotAsciiAlphaNum, TooLong, empty, notAsciiAlphaNum, tooLong)
import Wrapped.Validated as Wrapped

newtype Nickname = Nickname String

derive instance eqNickname :: Eq Nickname

derive instance newtypeNickname :: Newtype Nickname _

type NicknameError = Variant
    ( empty :: Empty
    , tooLong :: TooLong
    , notAsciiAlphaNum :: NotAsciiAlphaNum
    )

maxLength :: Int
maxLength = 40

create :: String -> Validated (NonEmptyList NicknameError) Nickname
create nickname =
    Wrapped.create trim [empty, tooLong maxLength, notAsciiAlphaNum]
    Nickname nickname

create' :: String -> Either (NonEmptyList NicknameError) Nickname
create' = create >>> toEither

fromNonEmpty ::
    NonEmptyString -> Validated (NonEmptyList NicknameError) Nickname
fromNonEmpty = toString >>> create

fromNonEmpty' ::
    NonEmptyString -> Either (NonEmptyList NicknameError) Nickname
fromNonEmpty' = toString >>> create'
