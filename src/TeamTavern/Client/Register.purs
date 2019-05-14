module TeamTavern.Client.Register (Slot, register) where

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
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Script.Cookie (hasPlayerIdCookie)
import TeamTavern.Client.Script.Navigate (navigate, navigate_)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Player.Register.SendResponse as Register
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Action
    = Init
    | EmailInput String
    | NicknameInput String
    | PasswordInput String
    | Register Event

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

type ChildSlots =
    ( home :: NavigationAnchor.Slot Unit
    , signInAnchor :: NavigationAnchor.Slot Unit
    , codeAnchor :: NavigationAnchor.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
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
    [ HP.class_ $ ClassName "single-form", HE.onSubmit $ Just <<< Register ]
    [ HH.h2_
        [ HH.text "Register to "
        , navigationAnchor (SProxy :: SProxy "home")
            { path: "/", content: HH.text "TeamTavern" }
        ]
    , HH.div_
        [ HH.label
            [ HP.for "nickname" ]
            [ HH.text "Nickname" ]
        , HH.input
            [ HP.id_ "nickname"
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
    , HH.div_
        [ HH.label
            [ HP.for "email" ]
            [ HH.text "Email address" ]
        , HH.input
            [ HP.id_ "email"
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
    , HH.div_
        [ HH.label
            [ HP.for "password" ]
            [ HH.text "Password" ]
        , HH.input
            [ HP.id_ "password"
            , HP.type_ InputPassword
            , HE.onValueInput $ Just <<< PasswordInput
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass passwordError ]
            [ HH.text $ "The password mush have at least 8 characters."
            ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "primary"
        , HP.disabled $ email == "" || nickname == "" || password == ""
        ]
        [ HH.text "Register" ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    , HH.p_
        [ HH.text "Already have an account?"
        , navigationAnchor (SProxy :: SProxy "signInAnchor")
            { path: "/signin", content: HH.text " Sign in." }
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

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    isSignedIn <- H.liftEffect hasPlayerIdCookie
    if isSignedIn
        then H.liftEffect $ navigate_ "/"
        else pure unit
    pure unit
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
        , initialize = Just Init
        }
    }

register :: forall query children left.
    HH.ComponentHTML query (register :: Slot Unit | children) (Async left)
register = HH.slot (SProxy :: SProxy "register") unit component unit absurd
