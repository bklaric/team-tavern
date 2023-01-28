module TeamTavern.Server.Player.Domain.Nickname where

import Prelude

import Async (Async)
import Async.Validated as AsyncVal
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as Nea
import Data.Bifunctor (lmap)
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.Validated as Validated
import Data.Variant (Variant, inj)
import Jarilo (badRequest_)
import Jarilo.Shared.Component (class Component)
import TeamTavern.Server.Infrastructure.Error (Terror(..), ValidatedTerrorNeaVar)
import TeamTavern.Server.Infrastructure.Response (BadRequestTerror)
import Type.Proxy (Proxy(..))
import Wrapped.String (Empty, NotAsciiAlphaNumSpecial, TooLong, empty, notAsciiAlphaNumSpecial, tooLong)
import Wrapped.Validated as Wrapped

newtype Nickname = Nickname String

derive instance Newtype Nickname _

derive newtype instance Show Nickname

derive newtype instance Component Nickname

type NicknameError = Variant
    ( empty :: Empty
    , tooLong :: TooLong
    , notAsciiAlphaNumSpecial :: NotAsciiAlphaNumSpecial
    )

type NicknameErrors = NonEmptyArray NicknameError

maxLength :: Int
maxLength = 40

validateNickname :: ∀ errors.
    String -> ValidatedTerrorNeaVar (nickname :: {} | errors) Nickname
validateNickname nickname =
    let validators = [empty, tooLong maxLength, notAsciiAlphaNumSpecial]
    in Wrapped.create trim validators Nickname nickname
    # Validated.lmap \(errors :: NicknameErrors) -> Terror
        (Nea.singleton $ inj (Proxy :: _ "nickname") {})
        [ "Nickname is invalid: " <> nickname
        , "Failed with following errors: " <> show errors
        ]

validateNickname' :: ∀ other errors. String -> Async (BadRequestTerror (Variant (nickname :: {} | other)) errors) Nickname
validateNickname' nickname =
    let validators = [empty, tooLong maxLength, notAsciiAlphaNumSpecial]
    in Wrapped.create trim validators Nickname nickname
    # AsyncVal.fromValidated
    # lmap \errors -> Terror
        (badRequest_ $ inj (Proxy :: _ "nickname") {})
        [ "Error validating nickname: " <> nickname
        , "Failed with following errors: " <> show (errors :: NonEmptyArray NicknameError)
        ]
