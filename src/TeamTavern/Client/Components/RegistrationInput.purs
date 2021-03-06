module TeamTavern.Client.Components.RegistrationInput (Input, Output, Slot, emptyInput, registrationInput) where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Record as Record
import TeamTavern.Client.Components.Input (inputError, inputGroup, inputLabel, requiredTextLineInput)
import TeamTavern.Client.Components.PasswordInput (passwordInput)

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

render :: forall left slots. State -> H.ComponentHTML Action slots (Async left)
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
        [ inputLabel "fas fa-signature" "Nickname"
        , requiredTextLineInput nickname UpdateNickname
        ]
        <> inputError nicknameError """Nickname cannot be more than 40 characters long
            and can only contain alphanumeric characters, dashes, underscores and dots."""
        <> inputError nicknameTaken "This nickname is already taken, please pick another one."
    , inputGroup $
        [ inputLabel "fas fa-key" "Password"
        , passwordInput password passwordShown UpdatePassword TogglePasswordVisibility
        ]
        <> inputError passwordError "Password must have at least 8 characters."
    ]

stateToOutput :: State -> Output
stateToOutput { nickname, password } = { nickname, password }

handleAction :: forall slots left. Action -> H.HalogenM State Action slots Output (Async left) Unit
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

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: Record.insert (SProxy :: SProxy "passwordShown") false
    , render
    , eval: H.mkEval $ H.defaultEval
        { receive = Just <<< Receive
        , handleAction = handleAction
        }
    }

registrationInput
    :: forall action children left
    .  Input
    -> (Output -> action)
    -> HH.ComponentHTML action (registrationInput :: Slot | children) (Async left)
registrationInput input handleOutput =
    HH.slot (SProxy :: SProxy "registrationInput") unit component input (Just <<< handleOutput)
