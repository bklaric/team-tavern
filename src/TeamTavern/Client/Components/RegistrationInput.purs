module TeamTavern.Client.Components.RegistrationInput (Input, Output, Slot, emptyInput, registrationInput) where

import Prelude

import Async (Async)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Record as Record
import Record.Extra (pick)
import TeamTavern.Client.Components.Input (inputError, inputGroup, inputLabel_, requiredTextLineInputNamed)
import TeamTavern.Client.Components.PasswordInput (passwordInput)
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

type Output =
    { email :: String
    , nickname :: String
    , password :: String
    }

type State =
    { email :: String
    , nickname :: String
    , password :: String
    , passwordShown :: Boolean
    , emailError :: Boolean
    , nicknameError :: Boolean
    , passwordError :: Boolean
    , emailTaken :: Boolean
    , nicknameTaken :: Boolean
    }

data Action
    = Receive Input
    | UpdateEmail String
    | UpdateNickname String
    | UpdatePassword String
    | TogglePasswordVisibility

type Slot = Slot_O_ Output

render :: ∀ left slots. State -> H.ComponentHTML Action slots (Async left)
render
    { email
    , nickname
    , password
    , passwordShown
    , emailError
    , nicknameError
    , passwordError
    , emailTaken
    , nicknameTaken
    } =
    HH.div_
    [ inputGroup $
        [ inputLabel_ "Email"
        , requiredTextLineInputNamed "email" email UpdateEmail
        ]
        <> inputError emailError "This doesn't look like a valid email address."
        <> inputError emailTaken "This email is already taken, please pick another one."
    , inputGroup $
        [ inputLabel_ "Nickname"
        , requiredTextLineInputNamed "nickname" nickname UpdateNickname
        ]
        <> inputError nicknameError """Nickname cannot be more than 40 characters long
            and can only contain alphanumeric characters, dashes, underscores and dots."""
        <> inputError nicknameTaken "This nickname is already taken, please pick another one."
    , inputGroup $
        [ inputLabel_ "Password"
        , passwordInput password passwordShown UpdatePassword TogglePasswordVisibility
        ]
        <> inputError passwordError "Password must have at least 8 characters."
    ]

handleAction :: ∀ slots left. Action -> H.HalogenM State Action slots Output (Async left) Unit
handleAction (Receive input) =
    H.modify_ $ Record.merge input
handleAction (UpdateEmail email) = do
    state <- H.modify _ { email = email }
    H.raise $ pick state
handleAction (UpdateNickname nickname) = do
    state <- H.modify _ { nickname = nickname }
    H.raise $ pick state
handleAction (UpdatePassword password) = do
    state <- H.modify _ { password = password }
    H.raise $ pick state
handleAction TogglePasswordVisibility =
    H.modify_ \state -> state { passwordShown = not state.passwordShown }

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

component :: ∀ query left.
    H.Component query Input Output (Async left)
component = H.mkComponent
    { initialState: Record.insert (Proxy :: _ "passwordShown") false
    , render
    , eval: H.mkEval $ H.defaultEval
        { receive = Just <<< Receive
        , handleAction = handleAction
        }
    }

registrationInput
    :: ∀ action children left
    .  Input
    -> (Output -> action)
    -> HH.ComponentHTML action (registrationInput :: Slot | children) (Async left)
registrationInput input handleOutput =
    HH.slot (Proxy :: _ "registrationInput") unit component input handleOutput
