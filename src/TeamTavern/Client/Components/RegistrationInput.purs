module TeamTavern.Client.Components.RegistrationInput (Input, Output, Slot, emptyInput, registrationInput) where

import Prelude

import Async (Async)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Input (inputError, inputGroup, inputLabel_, requiredTextLineInputNamed)
import TeamTavern.Client.Components.PasswordInput (passwordInput_)
import TeamTavern.Client.Shared.Slot (Slot_O_)
import Type.Proxy (Proxy(..))

type Input =
    { email :: String
    , nickname :: String
    , password :: String
    , emailError :: Boolean
    , nicknameError :: Boolean
    , passwordError :: Boolean
    , emailTaken :: Boolean
    , nicknameTaken :: Boolean
    }

type Output = Input

type Slot = Slot_O_ Input

component :: forall q m. H.Component q Input Output m
component = Hooks.component \{outputToken} input -> Hooks.do
    let { email, emailError, emailTaken
        , nickname, nicknameError, nicknameTaken
        , password, passwordError
        } = input
    let raise update = Hooks.raise outputToken $ update input
    Hooks.pure $
        HH.div_
        [ inputGroup $
            [ inputLabel_ "Email"
            , requiredTextLineInputNamed "email" email (\value -> raise _ {email = value})
            ]
            <> inputError emailError "This doesn't look like a valid email address."
            <> inputError emailTaken "This email is already taken, please pick another one."
        , inputGroup $
            [ inputLabel_ "Nickname"
            , requiredTextLineInputNamed "nickname" nickname (\value -> raise _ {nickname = value})
            ]
            <> inputError nicknameError """Nickname cannot be more than 40 characters long
                and can only contain alphanumeric characters, dashes, underscores and dots."""
            <> inputError nicknameTaken "This nickname is already taken, please pick another one."
        , inputGroup $
            [ inputLabel_ "Password"
            , passwordInput_ password (\value -> raise _ {password = value})
            ]
            <> inputError passwordError "Password must have at least 8 characters."
        ]

emptyInput :: Input
emptyInput =
    { email: ""
    , nickname: ""
    , password: ""
    , emailError: false
    , nicknameError: false
    , passwordError: false
    , emailTaken: false
    , nicknameTaken: false
    }

registrationInput
    :: ∀ action children left
    .  Input
    -> (Output -> action)
    -> HH.ComponentHTML action (registrationInput :: Slot | children) (Async left)
registrationInput input handleOutput =
    HH.slot (Proxy :: _ "registrationInput") unit component input handleOutput
