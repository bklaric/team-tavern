module TeamTavern.Client.Components.RegistrationInput (Input, Output, Slot, emptyInput, registrationInput) where

import Prelude

import Async (Async)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Input (inputError, inputGroup, inputLabel_, requiredTextLineInputNamed)
import TeamTavern.Client.Components.InputError as InputError
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
            <> InputError.emailError emailError
            <> InputError.emailTaken emailTaken
        , inputGroup $
            [ inputLabel_ "Nickname"
            , requiredTextLineInputNamed "nickname" nickname (\value -> raise _ {nickname = value})
            ]
            <> InputError.nicknameError nicknameError
            <> InputError.nicknameTaken nicknameTaken
        , inputGroup $
            [ inputLabel_ "Password"
            , passwordInput_ password (\value -> raise _ {password = value})
            ]
            <> InputError.passwordError passwordError
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
