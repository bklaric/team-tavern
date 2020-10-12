module TeamTavern.Client.Pages.Register where

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
import Halogen.HTML.Properties (ButtonType(..), InputType(..))
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Script.Cookie (hasPlayerIdCookie)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Navigate (navigate, navigateReplace_, navigateWithEvent_)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Player.Register.SendResponse as Register
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (MouseEvent)

data Action
    = Init
    | EmailInput String
    | NicknameInput String
    | PasswordInput String
    | TogglePasswordVisibility
    | Register Event
    | Navigate String MouseEvent

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
    , otherError :: Boolean
    , submitting :: Boolean
    }

type Slot = H.Slot (Const Void) Void

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
    , otherError
    , submitting
    } = HH.form
    [ HP.class_ $ HH.ClassName "form"
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
        , HH.div [ HP.class_ $ HH.ClassName "password-input-container" ]
            [ HH.input
                [ HP.id_ "password"
                , HP.class_ $ HH.ClassName "password-input"
                , HP.type_
                    if passwordShown
                    then InputText
                    else InputPassword
                , HE.onValueInput $ Just <<< PasswordInput
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
            [ HH.text $ "The password mush have at least 8 characters."
            ]
        ]
    , HH.button
        [ HP.class_ $ HH.ClassName "form-submit-button"
        , HP.disabled $
            email == "" || nickname == "" || password == "" || submitting
        ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-user-check button-icon" ] []
        , HH.text $
            if submitting
            then "Creating account..."
            else "Create account"
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
handleAction Init = do
    isSignedIn <- hasPlayerIdCookie
    if isSignedIn
        then navigateReplace_ "/"
        else pure unit
    H.liftEffect do
        setMetaTitle "Create account | TeamTavern"
        setMetaDescription "Create your TeamTavern account."
        setMetaUrl
handleAction (EmailInput email) =
    H.modify_ (_ { email = email })
handleAction (NicknameInput nickname) =
    H.modify_ (_ { nickname = nickname })
handleAction (PasswordInput password) =
    H.modify_ (_ { password = password })
handleAction TogglePasswordVisibility =
    H.modify_ (\state -> state { passwordShown = not state.passwordShown })
handleAction (Register event) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { emailError     = false
        , nicknameError  = false
        , passwordError  = false
        , emailTaken     = false
        , nicknameTaken  = false
        , otherError     = false
        , submitting     = true
        })
    H.put state
    newState <- H.lift $ sendRegisterRequest state
    case newState of
        Right content -> navigate content "/welcome"
        Left newState' -> H.put newState' { submitting = false }
handleAction (Navigate url event) =
    navigateWithEvent_ url event

component :: forall query input output left.
    H.Component HH.HTML query input output (Async left)
component = H.mkComponent
    { initialState: const
        { email: ""
        , nickname: ""
        , password: ""
        , passwordShown: false
        , emailError: false
        , nicknameError: false
        , passwordError: false
        , emailTaken: false
        , nicknameTaken: false
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Init
        , handleAction = handleAction
        }
    }

register :: forall query children left.
    HH.ComponentHTML query (register :: Slot Unit | children) (Async left)
register =
    HH.slot (SProxy :: SProxy "register") unit component unit absurd
