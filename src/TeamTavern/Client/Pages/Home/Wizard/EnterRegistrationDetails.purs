module TeamTavern.Client.Pages.Home.Wizard.EnterRegistrationDetails where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..), InputType(..))
import Halogen.HTML.Properties as HP
import Record.Builder as Record
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass)

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

type Slot = H.Slot (Const Void) Output Unit

render :: forall left slots. State -> H.ComponentHTML Action slots (Async left)
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
    [ HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "nickname" ]
            [ HH.text "Nickname" ]
        , HH.input
            [ HP.id_ "nickname"
            , HP.class_ $ HH.ClassName "text-line-input"
            , HP.value nickname
            , HE.onValueInput $ Just <<< UpdateNickname
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass nicknameError ]
            [ HH.text
                $ "The nickname can contain only alphanumeric characters and "
                <> "cannot be more than 40 characters long." ]
        , HH.p
            [ HP.class_ $ inputErrorClass nicknameTaken ]
            [ HH.text
                "This nickname is already taken, please pick another one." ]
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "email" ]
            [ HH.text "Email address" ]
        , HH.input
            [ HP.id_ "email"
            , HP.class_ $ HH.ClassName "text-line-input"
            , HP.value email
            , HE.onValueInput $ Just <<< UpdateEmail
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass emailError ]
            [ HH.text
                $  "This does not look like a valid email. "
                <> "Please check and try again."
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass emailTaken ]
            [ HH.text "This email is already taken, please pick another one." ]
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "password" ]
            [ HH.text "Password" ]
        , HH.div [ HP.class_ $ HH.ClassName "password-input-container" ]
            [ HH.input
                [ HP.id_ "password"
                , HP.class_ $ HH.ClassName "password-input"
                , HP.type_
                    if passwordShown
                    then InputText
                    else InputPassword
                , HP.value password
                , HE.onValueInput $ Just <<< UpdatePassword
                ]
            , HH.button
                [ HP.class_ $ HH.ClassName "password-input-button"
                , HP.type_ ButtonButton
                , HP.title
                    if passwordShown
                    then "Hide password"
                    else "Show password"
                , HE.onClick $ Just <<< const TogglePasswordVisibility
                ]
                [ HH.i
                    [ HP.class_ $ HH.ClassName
                        if passwordShown
                        then "fas fa-eye-slash"
                        else "fas fa-eye"
                    ]
                    []
                ]
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass passwordError ]
            [ HH.text $ "The password mush have at least 8 characters." ]
        ]
    ]

stateToOutput :: State -> Output
stateToOutput { email, nickname, password } = { email, nickname, password }

handleAction :: forall slots left.
    Action -> H.HalogenM State Action slots Output (Async left) Unit
handleAction (Receive input) =
    H.modify_ _
        { emailError = input.emailError
        , nicknameError = input.nicknameError
        , passwordError = input.passwordError
        , emailTaken = input.emailTaken
        , nicknameTaken = input.nicknameTaken
        }
handleAction (UpdateEmail email) = do
    state <- H.modify _ { email = email }
    H.raise $ stateToOutput state
handleAction (UpdateNickname nickname) = do
    state <- H.modify _ { nickname = nickname }
    H.raise $ stateToOutput state
handleAction (UpdatePassword password) = do
    state <- H.modify _ { password = password }
    H.raise $ stateToOutput state
handleAction TogglePasswordVisibility =
    H.modify_ (\state -> state { passwordShown = not state.passwordShown })

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

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState:
        Record.build (Record.insert (SProxy :: SProxy "passwordShown") false)
    , render
    , eval: H.mkEval $ H.defaultEval
        { receive = Just <<< Receive
        , handleAction = handleAction
        }
    }

enterRegistrationDetails
    :: forall action children left
    .  Input
    -> (Output -> Maybe action)
    -> HH.ComponentHTML action (enterRegistrationDetails :: Slot | children) (Async left)
enterRegistrationDetails input handleOutput =
    HH.slot (SProxy :: SProxy "enterRegistrationDetails") unit component input handleOutput
