module TeamTavern.Client.Components.RegistrationInput (Input, Output, Slot, emptyInput, registrationInput) where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Record as Record
import TeamTavern.Client.Components.Input (inputError, inputGroup, requiredTextLineInput)
import TeamTavern.Client.Components.PasswordInput (passwordInput)
import TeamTavern.Client.Snippets.Class as HS
import Type.Proxy (Proxy(..))

type Input =
    { nickname :: String
    , password :: String
    , nicknameError :: Boolean
    , passwordError :: Boolean
    , nicknameTaken :: Boolean
    }

type Output =
    { nickname :: String
    , password :: String
    }

type State =
    { nickname :: String
    , password :: String
    , passwordShown :: Boolean
    , nicknameError :: Boolean
    , passwordError :: Boolean
    , nicknameTaken :: Boolean
    }

data Action
    = Receive Input
    | UpdateNickname String
    | UpdatePassword String
    | TogglePasswordVisibility

type Slot = H.Slot (Const Void) Output Unit

render :: ∀ left slots. State -> H.ComponentHTML Action slots (Async left)
render
    { nickname
    , password
    , passwordShown
    , nicknameError
    , passwordError
    , nicknameTaken
    } =
    HH.div_
    [ inputGroup $
        [ HH.label [ HS.class_ "input-label" ] [ HH.text "Nickname" ]
        , requiredTextLineInput nickname UpdateNickname
        ]
        <> inputError nicknameError """Nickname cannot be more than 40 characters long
            and can only contain alphanumeric characters, dashes, underscores and dots."""
        <> inputError nicknameTaken "This nickname is already taken, please pick another one."
    , inputGroup $
        [ HH.label [ HS.class_ "input-label" ] [ HH.text "Password" ]
        , passwordInput password passwordShown UpdatePassword TogglePasswordVisibility
        ]
        <> inputError passwordError "Password must have at least 8 characters."
    ]

stateToOutput :: State -> Output
stateToOutput { nickname, password } = { nickname, password }

handleAction :: ∀ slots left. Action -> H.HalogenM State Action slots Output (Async left) Unit
handleAction (Receive input) =
    H.modify_ _
        { nickname = input.nickname
        , password = input.password
        , nicknameError = input.nicknameError
        , passwordError = input.passwordError
        , nicknameTaken = input.nicknameTaken
        }
handleAction (UpdateNickname nickname) = do
    state <- H.modify _ { nickname = nickname }
    H.raise $ stateToOutput state
handleAction (UpdatePassword password) = do
    state <- H.modify _ { password = password }
    H.raise $ stateToOutput state
handleAction TogglePasswordVisibility =
    H.modify_ \state -> state { passwordShown = not state.passwordShown }

emptyInput :: Input
emptyInput =
    { nickname: ""
    , password: ""
    , nicknameError: false
    , passwordError: false
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
