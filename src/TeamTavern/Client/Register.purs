module TeamTavern.Client.Register (Query, Slot, register) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (bimap, lmap)
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
import TeamTavern.Client.Snippets.ErrorClasses (errorClass, inputErrorClass, otherErrorClass)
import TeamTavern.Player.Register.SendResponse as Register
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Query send
    = Init send
    | EmailInput String send
    | NicknameInput String send
    | PasswordInput String send
    | Register Event send

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

type Slot = H.Slot Query Void

type ChildSlots =
    ( home :: NavigationAnchor.Slot Unit
    , signInAnchor :: NavigationAnchor.Slot Unit
    , codeAnchor :: NavigationAnchor.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Query ChildSlots (Async left)
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
    [ HE.onSubmit $ HE.input Register ]
    [ HH.h2_
        [ HH.text "Register to "
        , navigationAnchor (SProxy :: SProxy "home")
            { path: "/", text: "TeamTavern" }
        ]
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
            [ HH.li_ [ HH.text "Contain only alphanumeric characters." ]
            , HH.li_ [ HH.text "Be no more than 40 characters long." ]
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass nicknameTaken ]
            [ HH.text "This nickname is taken, please pick another one." ]
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
            [ HH.text
                $  "This does not look like a valid email. "
                <> "Please check and try again."
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass emailTaken ]
            [ HH.text "This email is taken, please pick another one." ]
        ]
    , HH.div_
        [ HH.label
            [ HP.class_ $ errorClass passwordError, HP.for "password" ]
            [ HH.text "Password" ]
        , HH.input
            [ HP.id_ "password"
            , HP.class_ $ errorClass passwordError
            , HP.type_ InputPassword
            , HE.onValueInput $ HE.input PasswordInput
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
            { path: "/signin", text: " Sign in." }
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

eval :: forall void.
    Query ~> H.HalogenM State Query ChildSlots Void (Async void)
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
eval (PasswordInput password send) =
    H.modify_ (_ { password = password }) <#> const send
eval (Register event send) = do
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
    pure send

component :: forall input left.
    H.Component HH.HTML Query input Void (Async left)
component = H.component
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
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Init
    , finalizer: Nothing
    }

register :: forall query children left.
    HH.ComponentHTML query (register :: Slot Unit | children) (Async left)
register = HH.slot (SProxy :: SProxy "register") unit component unit absurd
