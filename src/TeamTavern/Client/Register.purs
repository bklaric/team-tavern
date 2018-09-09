module TeamTavern.Client.Register where

import Prelude

import Async (Async)
import Async as A
import Browser.Async.Fetch as FA
import Browser.Async.Fetch.Response as FARes
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Symbol (SProxy(..))
import Data.Variant as V
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log, logShow)
import Error.Class as E
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as J
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Script.Cookie (hasPlayerIdCookie)
import TeamTavern.Client.Script.Navigate (navigate, navigate_)
-- import TeamTavern.Client.SignIn (errorClass, inputErrorClass, otherErrorClass)
-- import TeamTavern.Player.Register.Run.CreateResponse (BadRequestContent, OkContent)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Query send
    = Init send
    | EmailInput String send
    | NicknameInput String send
    | Register Event send

type State =
    { email :: String
    , nickname :: String
    , emailError :: Boolean
    , nicknameError :: Boolean
    , emailTaken :: Boolean
    , nicknameTaken :: Boolean
    , otherError :: Boolean
    }

type Message = Void

type Slot = H.Slot Query Message

type ChildSlots =
    ( signInAnchor :: NavigationAnchor.Slot Unit
    , codeAnchor :: NavigationAnchor.Slot Unit
    )

_signInAnchor = SProxy :: SProxy "signInAnchor"

_codeAnchor = SProxy :: SProxy "codeAnchor"

initialState :: State
initialState =
    { email: ""
    , nickname: ""
    , emailError: false
    , nicknameError: false
    , emailTaken: false
    , nicknameTaken: false
    , otherError: false
    }

render :: forall m. MonadEffect m => State -> H.ComponentHTML Query ChildSlots m
render
    { email
    , nickname
    , emailError
    , nicknameError
    , emailTaken
    , nicknameTaken
    , otherError
    } = HH.form
    [ HE.onSubmit $ HE.input Register ]
    [ HH.h2_ [ HH.text "Register to TeamTavern" ]
    , HH.div_
        [ HH.label
            [ HP.class_ $ errorClass nicknameError, HP.for "nickname" ]
            [ HH.text "Nickname" ]
        , HH.input
            [ HP.id_ "nickname"
            , HP.class_ $ errorClass nicknameError
            , HE.onValueInput $ HE.input NicknameInput
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass nicknameError ]
            [ HH.text "Please enter a valid nickname. The nickname must: " ]
        , HH.ul
            [ HP.class_ $ inputErrorClass nicknameError ]
            [ HH.li_ [ HH.text "Have no spaces" ]
            , HH.li_ [ HH.text "Be some shit I forgot" ]
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass nicknameTaken ]
            [ HH.text "This nickname is taken, pick another one." ]
        ]
    , HH.div_
        [ HH.label
            [ HP.class_ $ errorClass emailError, HP.for "email" ]
            [ HH.text "Email" ]
        , HH.input
            [ HP.id_ "email"
            , HP.class_ $ errorClass emailError
            , HE.onValueInput $ HE.input EmailInput
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass emailError ]
            [ HH.text "This does not look like a valid email. Jesus Christ, how dense are you?" ]
        , HH.p
            [ HP.class_ $ inputErrorClass emailTaken ]
            [ HH.text "This email is taken, pick another one." ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "primary"
        , HP.disabled $ email == "" || nickname == ""
        ]
        [ HH.text "Register" ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Lmao, something else got fucked and you're shit out of luck, mate!"]
    , HH.slot _signInAnchor unit navigationAnchor
        { path: "/signin", text: "Sign in" } absurd
    , HH.slot _codeAnchor unit navigationAnchor
        { path: "/code", text: "Get a sign in code" } absurd
    ]

eval :: forall void.
    Query ~> H.HalogenM State Query ChildSlots Message (Async void)
eval (Init send) = do
    isSignedIn <- H.liftEffect hasPlayerIdCookie
    if isSignedIn
        then H.liftEffect $ navigate_ "/"
        else pure unit
    pure send
eval (EmailInput email send) =
    H.modify_ (_ { email = email }) <#> const send
eval (NicknameInput nickname send) =
    H.modify_ (_ { nickname = nickname }) <#> const send
eval (Register event send) = let
    setEmailError       = _ { emailError       = true }
    setNicknameError    = _ { nicknameError    = true }
    setEmailTaken       = _ { emailTaken       = true }
    setNicknameTaken    = _ { nicknameTaken    = true }
    setSendEmailError   = _ { sendEmailError   = true }
    setOtherError       = _ { otherError       = true }
    in do
    H.liftEffect $ preventDefault event
    resetState @ { email, nickname } <- H.gets (_
        { emailError     = false
        , nicknameError  = false
        , emailTaken     = false
        , nicknameTaken  = false
        , otherError     = false
        })
    response' <- H.lift $ A.attempt $ FA.fetch
        ("http://localhost:8080/players")
        (  FA.method := POST
        <> FA.body := J.writeJSON { email, nickname }
        <> FA.credentials := FA.Include
        )
    newState <- case response' of
        Left error -> do
            log $ E.message error
            pure $ setOtherError resetState
        Right response -> H.lift
            case FARes.status response of
            200 -> FARes.text response <#> J.readJSON >>=
                case _ of
                Left errors -> do
                    logShow errors
                    pure $ setOtherError resetState
                Right (content :: OkContent) -> do
                    H.liftEffect $ navigate content "/welcome"
                    pure resetState
            400 -> FARes.text response <#> J.readJSON >>=
                case _ of
                Left errors -> do
                    logShow errors
                    pure $ setOtherError resetState
                Right (error :: BadRequestContent) -> V.match
                    { invalidIdentifiers: foldl (\state -> V.match
                        { invalidEmail:
                            const $ setEmailError state
                        , invalidNickname:
                            const $ setNicknameError state
                        })
                        resetState
                    , emailTaken:
                        const $ setEmailTaken resetState
                    , nicknameTaken:
                        const $ setNicknameTaken resetState
                    }
                    error
                    # pure
            _ -> pure $ setOtherError resetState
    H.put newState
    pure send

register :: forall input void.
    H.Component HH.HTML Query input Message (Async void)
register = H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Init
    , finalizer: Nothing
    }
