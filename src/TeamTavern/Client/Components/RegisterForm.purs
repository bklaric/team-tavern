module TeamTavern.Client.Components.RegisterForm where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Symbol (SProxy(..))
import Data.Variant (match)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Script.Navigate (navigate, navigateWithEvent_)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Player.Register.SendResponse as Register
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (MouseEvent)

data Action
    = EmailInput String
    | NicknameInput String
    | PasswordInput String
    | Register Event
    | Navigate String MouseEvent

type State =
    { email :: String
    , nickname :: String
    , password :: String
    , emailError :: Boolean
    , nicknameError :: Boolean
    , passwordError :: Boolean
    , emailTaken :: Boolean
    , nicknameTaken :: Boolean
    , otherError :: Boolean
    }

type Slot = H.Slot (Const Void) Void

render :: forall left slots. State -> H.ComponentHTML Action slots (Async left)
render
    { email
    , nickname
    , password
    , emailError
    , nicknameError
    , passwordError
    , emailTaken
    , nicknameTaken
    , otherError
    } = HH.form
    [ HP.class_ $ HH.ClassName "register-form"
    , HE.onSubmit $ Just <<< Register
    ]
    [ HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
        [ HH.text "Create your "
        , HH.a
            [ HP.href "/"
            , HE.onClick $ Just <<< Navigate "/"
            ]
            [ HH.text "TeamTavern" ]
        , HH.text " account"
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "nickname" ]
            [ HH.text "Nickname" ]
        , HH.input
            [ HP.id_ "nickname"
            , HP.class_ $ HH.ClassName "text-line-input"
            , HE.onValueInput $ Just <<< NicknameInput
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
            , HE.onValueInput $ Just <<< EmailInput
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
        , HH.input
            [ HP.id_ "password"
            , HP.class_ $ HH.ClassName "text-line-input"
            , HP.type_ InputPassword
            , HE.onValueInput $ Just <<< PasswordInput
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass passwordError ]
            [ HH.text $ "The password mush have at least 8 characters."
            ]
        ]
    , HH.button
        [ HP.class_ $ HH.ClassName "form-submit-button"
        , HP.disabled $ email == "" || nickname == "" || password == ""
        ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user-check button-icon" ] []
        , HH.text "Create account"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    , HH.p
        [ HP.class_ $ HH.ClassName "form-bottom-text" ]
        [ HH.text "Already have an account? "
        , HH.a
            [ HP.href "/signin"
            , HE.onClick $ Just <<< Navigate "/signin"
            ]
            [ HH.text "Sign in." ]
        ]
    ]

sendRegisterRequest :: forall left.
    State -> Async left (Either State Register.OkContent)
sendRegisterRequest state @ { email, nickname, password } = Async.unify do
    response <- Fetch.fetch "/api/players"
        (  Fetch.method := POST
        <> Fetch.body := Json.writeJSON { email, nickname, password }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Left $ state { otherError = true })
    newState <- case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap (const $ Left $ state { otherError = true }) Right
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Left $ state { otherError = true })
                (\(error :: Register.BadRequestContent) -> Left $ match
                    { invalidModel: foldl (\state' -> match
                        { invalidEmail:
                            const $ state' { emailError = true }
                        , invalidNickname:
                            const $ state' { nicknameError = true }
                        , invalidPassword:
                            const $ state' { passwordError = true }
                        })
                        state
                    , emailTaken:
                        const $ state { emailTaken = true }
                    , nicknameTaken:
                        const $ state { nicknameTaken = true }
                    }
                    error)
        _ -> pure $ Left $ state { otherError = true }
    pure newState

handleAction :: forall slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction (EmailInput email) =
    H.modify_ (_ { email = email }) $> unit
handleAction (NicknameInput nickname) =
    H.modify_ (_ { nickname = nickname }) $> unit
handleAction (PasswordInput password) =
    H.modify_ (_ { password = password }) $> unit
handleAction (Register event) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { emailError     = false
        , nicknameError  = false
        , passwordError  = false
        , emailTaken     = false
        , nicknameTaken  = false
        , otherError     = false
        })
    newState <- H.lift $ sendRegisterRequest state
    case newState of
        Right content -> H.liftEffect $ navigate content "/welcome"
        Left newState' -> H.put newState'
    pure unit
handleAction (Navigate url event) =
    H.liftEffect $ navigateWithEvent_ url event

component :: forall query input output left.
    H.Component HH.HTML query input output (Async left)
component = H.mkComponent
    { initialState: const
        { email: ""
        , nickname: ""
        , password: ""
        , emailError: false
        , nicknameError: false
        , passwordError: false
        , emailTaken: false
        , nicknameTaken: false
        , otherError: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

registerForm :: forall query children left.
    HH.ComponentHTML query (registerForm :: Slot Unit | children) (Async left)
registerForm =
    HH.slot (SProxy :: SProxy "registerForm") unit component unit absurd
