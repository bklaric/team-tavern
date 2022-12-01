module TeamTavern.Server.Player.Domain.Nickname where

import Prelude

import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.Validated (Validated)
import Data.Validated.Label as Validated
import Data.Variant (Variant)
import Jarilo.Shared.Component (class Component)
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
    String -> Validated (NonEmptyList (Variant (nickname :: Array String | errors))) Nickname
validateNickname nickname =
    Wrapped.create trim [empty, tooLong maxLength, notAsciiAlphaNumSpecial] Nickname nickname
    # Validated.labelMap (Proxy :: _ "nickname") \(errors :: NicknameErrors) ->
        [ "Registration nickname is invalid: " <> nickname
        , "Failed with following errors: " <> show errors
        ]
