module TeamTavern.Server.Player.Domain.Nickname where

import Prelude

import Data.List.NonEmpty (singleton)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.Validated as Validated
import Data.Variant (Variant, inj)
import Jarilo.Shared.Component (class Component)
import TeamTavern.Server.Infrastructure.Error (TavernErrorMany(..), ValidatedTavern)
import Type.Proxy (Proxy(..))
import Wrapped.String (Empty, NotAsciiAlphaNumSpecial, TooLong, empty, notAsciiAlphaNumSpecial, tooLong)
import Wrapped.Validated as Wrapped

newtype Nickname = Nickname String

derive instance newtypeNickname :: Newtype Nickname _

derive newtype instance showNickname :: Show Nickname

derive newtype instance fromComponentNickname :: Component Nickname

type NicknameError = Variant
    ( empty :: Empty
    , tooLong :: TooLong
    , notAsciiAlphaNumSpecial :: NotAsciiAlphaNumSpecial
    )

type NicknameErrors = NonEmptyList NicknameError

maxLength :: Int
maxLength = 40

validateNickname :: forall errors.
    String -> ValidatedTavern (nickname :: {} | errors) Nickname
validateNickname nickname =
    let validators = [empty, tooLong maxLength, notAsciiAlphaNumSpecial]
    in Wrapped.create trim validators Nickname nickname
    # Validated.lmap \(errors :: NicknameErrors) -> TavernErrorMany
        (singleton $ inj (Proxy :: _ "nickname") {})
        [ "Registration nickname is invalid: " <> nickname
        , "Failed with following errors: " <> show errors
        ]
