module TeamTavern.Client.SignInCode (Query, Slot, signInCode) where

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
import Data.String (trim)
import Data.Symbol (SProxy(..))
import Data.Variant (match)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Script.Cookie (hasPlayerIdCookie)
import TeamTavern.Client.Script.Navigate (navigate, navigate_)
import TeamTavern.Client.Snippets.ErrorClasses (errorClass, inputErrorClass, otherErrorClass)
import TeamTavern.Session.Prepare.Response as Prepare
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

type Slot = H.Slot Query Void

type ChildSlots =
    ( home :: NavigationAnchor.Slot Unit
    , signInAnchor :: NavigationAnchor.Slot Unit
    , registerAnchor :: NavigationAnchor.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Query ChildSlots (Async left)
render
    { email
    , nickname
    , emailError
    , nicknameError
    , unknownIdentifiers
    , otherError
    } = HH.form
    [ HE.onSubmit $ HE.input RequestCode ]
    [ HH.h2_ [ HH.text "Get a sign in code for "
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
    , navigationAnchor (SProxy :: SProxy "signInAnchor")
        { path: "/signin", text: "Sign in" }
    , navigationAnchor (SProxy :: SProxy "registerAnchor")
        { path: "/register", text: "Register" }
    ]

sendCodeRequest :: forall left.
    State -> Async left (Either State { email :: String, nickname :: String })
sendCodeRequest state @ { email, nickname } = Async.unify do
    response <- Fetch.fetch "/api/sessions"
        (  Fetch.method := POST
        <> Fetch.body := Json.writeJSON { email, nickname }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Left $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure $ Right $ { email: trim email, nickname: trim nickname }
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Left $ state { otherError = true })
                (\(error :: Prepare.BadRequestContent) -> Left $ match
                    { invalidIdentifiers: foldl (\state' -> match
                        { invalidEmail:
                            const $ state' { emailError = true }
                        , invalidNickname:
                            const $ state' { nicknameError = true }
                        })
                        state
                    , unknownIdentifiers:
                        const $ state { unknownIdentifiers = true }
                    }
                    error)
        _ -> pure $ Left $ state { otherError = true }
    pure newState

eval :: forall left.
    Query ~> H.HalogenM State Query ChildSlots Void (Async left)
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
eval (RequestCode event send) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { emailError         = false
        , nicknameError      = false
        , unknownIdentifiers = false
        , otherError         = false
        })
    newState <- H.lift $ sendCodeRequest state
    case newState of
        Right content -> H.liftEffect $ navigate content "/codesent"
        Left newState' -> H.put newState'
    pure send

component :: forall input void.
    H.Component HH.HTML Query input Void (Async void)
component = H.component
    { initialState: const
        { email: ""
        , nickname: ""
        , emailError: false
        , nicknameError: false
        , unknownIdentifiers: false
        , otherError: false
        }
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Init
    , finalizer: Nothing
    }

signInCode :: forall query children left.
    HH.ComponentHTML query (signInCode :: Slot Unit | children) (Async left)
signInCode = HH.slot (SProxy :: SProxy "signInCode") unit component unit absurd
