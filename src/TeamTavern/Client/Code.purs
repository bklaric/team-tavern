module TeamTavern.Client.Code where

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
import Data.String (trim)
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
import TeamTavern.Client.SignIn (errorClass, inputErrorClass, otherErrorClass)
import TeamTavern.Player.Session.Prepare.Run.CreateResponse (BadRequestResponseContent)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Query send
    = Init send
    | EmailInput String send
    | NicknameInput String send
    | RequestCode Event send

type State =
    { email :: String
    , nickname :: String
    , emailError :: Boolean
    , nicknameError :: Boolean
    , unknownIdentifiers :: Boolean
    , otherError :: Boolean
    }

type Message = Void

type Slot = H.Slot Query Message

type ChildSlots =
    ( signInAnchor :: NavigationAnchor.Slot Unit
    , registerAnchor :: NavigationAnchor.Slot Unit
    )

_signInAnchor = SProxy :: SProxy "signInAnchor"

_registerAnchor = SProxy :: SProxy "registerAnchor"

initialState :: State
initialState =
    { email: ""
    , nickname: ""
    , emailError: false
    , nicknameError: false
    , unknownIdentifiers: false
    , otherError: false
    }

render :: forall m. MonadEffect m => State -> H.ComponentHTML Query ChildSlots m
render
    { email
    , nickname
    , emailError
    , nicknameError
    , unknownIdentifiers
    , otherError
    } = HH.form
    [ HE.onSubmit $ HE.input RequestCode ]
    [ HH.h2_ [ HH.text "Get a sign in code for TeamTavern" ]
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
        ]
    , HH.button
        [ HP.class_ $ ClassName "primary"
        , HP.disabled $ email == "" || nickname == ""
        ]
        [ HH.text "Send the code" ]
    , HH.p
        [ HP.class_ $ otherErrorClass unknownIdentifiers ]
        [ HH.text $ "This isn't a known combination of nickname and email. "
            <> "Please check if they are correct."
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Lmao, something else got fucked and you're shit out of luck, mate!" ]
    , HH.slot _signInAnchor unit navigationAnchor
        { path: "/signin", text: "Sign in" } absurd
    , HH.slot _registerAnchor unit navigationAnchor
        { path: "/register", text: "Register" } absurd
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
eval (RequestCode event send) = let
    setEmailError         = _ { emailError         = true }
    setNicknameError      = _ { nicknameError      = true }
    setUnknownIdentifiers = _ { unknownIdentifiers = true }
    setOtherError         = _ { otherError         = true }
    in do
    H.liftEffect $ preventDefault event
    resetState @ { email, nickname } <- H.gets (_
        { emailError         = false
        , nicknameError      = false
        , unknownIdentifiers = false
        , otherError         = false
        })
    response' <- H.lift $ A.attempt $ FA.fetch
        ("http://localhost:8080/players/by-nickname/" <> nickname <> "/sessions")
        (  FA.method := POST
        <> FA.body := J.writeJSON { email }
        <> FA.credentials := FA.Include
        )
    newState <- case response' of
        Left error -> do
            log $ E.message error
            pure $ setOtherError resetState
        Right response -> H.lift
            case FARes.status response of
            204 -> do
                H.liftEffect $ navigate
                    { email: trim email
                    , nickname: trim nickname
                    }
                    "/codesent"
                pure resetState
            400 -> FARes.text response <#> J.readJSON >>=
                case _ of
                Left errors -> do
                    logShow errors
                    pure $ setOtherError resetState
                Right (error :: BadRequestResponseContent) -> V.match
                    { invalidIdentifiers: foldl (\state -> V.match
                        { invalidEmail:
                            const $ setEmailError state
                        , invalidNickname:
                            const $ setNicknameError state
                        })
                        resetState
                    , unknownIdentifiers:
                        const $ setUnknownIdentifiers resetState
                    }
                    error
                    # pure
            _ -> pure $ setOtherError resetState
    H.put newState
    pure send

code :: forall input void.
    H.Component HH.HTML Query input Message (Async void)
code = H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Init
    , finalizer: Nothing
    }
