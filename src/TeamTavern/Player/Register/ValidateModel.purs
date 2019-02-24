module TeamTavern.Player.Register.ValidateModel
    ( Email
    , EmailError
    , unEmail
    , Nickname
    , NicknameError
    , unNickname
    , Password
    , PasswordError
    , unPassword
    , RegisterModel
    , RegisterModelError
    , ValidateModelError
    , validateModel) where

import Prelude

import Async (Async)
import Async.Validated (fromValidated) as Async
import Data.Bifunctor.Label (labelMap)
import Data.Either (fromRight)
import Data.List.Types (NonEmptyList)
import Data.Maybe (isJust)
import Data.String (trim)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (unicode)
import Data.Validated (Validated)
import Data.Validated.Label as Validated
import Data.Variant (SProxy(..), Variant)
import Partial.Unsafe (unsafePartial)
import TeamTavern.Player.Register.ReadDto (RegisterDto)
import Wrapped.String (Invalid, NotAsciiAlphaNum, TooLong, TooShort, Empty, empty, invalid, notAsciiAlphaNum, tooLong, tooShort)
import Wrapped.Validated as Wrapped

newtype Email = Email String

type EmailError = Variant (invalid :: Invalid, tooLong :: TooLong)

emailRegex :: Regex
emailRegex =
    regex """^[^\s@]+@[^\s@]+\.[^\s@]+$""" unicode # unsafePartial fromRight

createEmail :: String -> Validated (NonEmptyList EmailError) Email
createEmail email =
    Wrapped.create trim [invalid (match emailRegex >>> isJust), tooLong 254]
        Email email

unEmail :: Email -> String
unEmail (Email email) = email

newtype Nickname = Nickname String

type NicknameError = Variant
    ( empty :: Empty
    , tooLong :: TooLong
    , notAsciiAlphaNum :: NotAsciiAlphaNum
    )

maxNicknameLength :: Int
maxNicknameLength = 40

createNickname :: String -> Validated (NonEmptyList NicknameError) Nickname
createNickname nickname =
    Wrapped.create trim [empty, tooLong maxNicknameLength, notAsciiAlphaNum]
        Nickname nickname

unNickname :: Nickname -> String
unNickname (Nickname nickname) = nickname

newtype Password = Password String

type PasswordError = Variant (tooShort :: TooShort)

minPasswordLength :: Int
minPasswordLength = 8

createPassword :: String -> Validated (NonEmptyList PasswordError) Password
createPassword password =
    Wrapped.create identity [tooShort minPasswordLength] Password password

unPassword :: Password -> String
unPassword (Password password) = password

type RegisterModel =
    { email :: Email
    , nickname :: Nickname
    , password :: Password
    }

type RegisterModelError = Variant
    ( email :: NonEmptyList EmailError
    , nickname :: NonEmptyList NicknameError
    , password :: NonEmptyList PasswordError
    )

type ValidateModelError errors = Variant
    ( invalidModel ::
        { dto :: RegisterDto
        , errors :: NonEmptyList RegisterModelError
        }
    | errors )

validateModel :: forall errors.
    RegisterDto -> Async (ValidateModelError errors) RegisterModel
validateModel dto @ { email, nickname, password } = do
    { email: _, nickname: _, password: _ }
        <$> (createEmail email
            # Validated.label (SProxy :: SProxy "email"))
        <*> (createNickname nickname
            # Validated.label (SProxy :: SProxy "nickname"))
        <*> (createPassword password
            # Validated.label (SProxy :: SProxy "password"))
        # Async.fromValidated
        # labelMap (SProxy :: SProxy "invalidModel")
            { dto, errors: _ }
