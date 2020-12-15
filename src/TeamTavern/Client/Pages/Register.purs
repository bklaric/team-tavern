module TeamTavern.Client.Pages.Register (Slot, register) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
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
import TeamTavern.Client.Pages.Onboarding as Onboarding
import TeamTavern.Client.Script.Cookie (hasPlayerIdCookie)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigate, navigateReplace_, navigateWithEvent_)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Player.Register.ReadDto (RegisterDto)
import TeamTavern.Server.Player.Register.SendResponse as Register
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (MouseEvent)

data Action
    = Initialize
    | UpdateNickname String
    | UpdatePassword String
    | TogglePasswordVisibility
    | Register Event
    | Navigate String MouseEvent

type State =
    { nickname :: String
    , password :: String
    , passwordShown :: Boolean
    , nicknameError :: Boolean
    , passwordError :: Boolean
    , nicknameTaken :: Boolean
    , otherError :: Boolean
    , submitting :: Boolean
    }

type Slot = H.Slot (Const Void) Void

render :: forall left slots. State -> H.ComponentHTML Action slots (Async left)
render
    { nickname
    , password
    , passwordShown
    , nicknameError
    , passwordError
    , nicknameTaken
    , otherError
    , submitting
    } = HH.form
    [ HP.class_ $ HH.ClassName "form"
    , HE.onSubmit $ Just <<< Register
    ]
    [ HH.h1 [ HP.class_ $ HH.ClassName "form-heading" ]
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
            , HE.onValueInput $ Just <<< UpdateNickname
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass nicknameError ]
            [ HH.text
                $ "Nickname cannot be more than 40 characters long "
                <> "and can only contain alphanumeric characters, dashes, underscores and dots." ]
        , HH.p
            [ HP.class_ $ inputErrorClass nicknameTaken ]
            [ HH.text
                "This nickname is already taken, please pick another one." ]
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
            [ HH.text $ "Password must have at least 8 characters."
            ]
        ]
    , HH.button
        [ HP.class_ $ HH.ClassName "form-submit-button"
        , HP.disabled $ nickname == "" || password == "" || submitting
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

sendRegisterRequest :: forall left. State -> Async left (Maybe State)
sendRegisterRequest state @ { nickname, password } = Async.unify do
    response <- Fetch.fetch "/api/players"
        (  Fetch.method := POST
        <> Fetch.body := Json.writeJSON ({ nickname, password } :: RegisterDto)
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true })
                (\(error :: Register.BadRequestContent) -> Just $ match
                    { registration: foldl (\state' -> match
                        { invalidNickname:
                            const $ state' { nicknameError = true }
                        , invalidPassword:
                            const $ state' { passwordError = true }
                        })
                        state
                    , nicknameTaken:
                        const $ state { nicknameTaken = true }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

handleAction :: forall slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Initialize = do
    H.liftEffect $ whenM hasPlayerIdCookie $ navigateReplace_ "/"
    setMeta "Create account | TeamTavern" "Create your TeamTavern account."
handleAction (UpdateNickname nickname) =
    H.modify_ (_ { nickname = nickname })
handleAction (UpdatePassword password) =
    H.modify_ (_ { password = password })
handleAction TogglePasswordVisibility =
    H.modify_ (\state -> state { passwordShown = not state.passwordShown })
handleAction (Register event) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { nicknameError  = false
        , passwordError  = false
        , nicknameTaken  = false
        , otherError     = false
        , submitting     = true
        })
    H.put state
    newState <- H.lift $ sendRegisterRequest state
    case newState of
        Nothing -> navigate Onboarding.emptyInput "/onboarding/start"
        Just newState' -> H.put newState' { submitting = false }
handleAction (Navigate url event) =
    navigateWithEvent_ url event

component :: forall query input output left. H.Component HH.HTML query input output (Async left)
component = H.mkComponent
    { initialState: const
        { nickname: ""
        , password: ""
        , passwordShown: false
        , nicknameError: false
        , passwordError: false
        , nicknameTaken: false
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { initialize = Just Initialize
        , handleAction = handleAction
        }
    }

register :: forall query children left.
    HH.ComponentHTML query (register :: Slot Unit | children) (Async left)
register = HH.slot (SProxy :: SProxy "register") unit component unit absurd
