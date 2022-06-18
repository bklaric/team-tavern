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
import Data.Variant (match)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.RegistrationInput (registrationInput)
import TeamTavern.Client.Components.RegistrationInput as RegistrationInput
import TeamTavern.Client.Pages.Onboarding as Onboarding
import TeamTavern.Client.Script.Cookie (hasPlayerIdCookie)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigate, navigateReplace_, navigateWithEvent_)
import TeamTavern.Client.Snippets.ErrorClasses (otherErrorClass)
import TeamTavern.Routes.Player.RegisterPlayer as RegisterPlayer
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (MouseEvent)
import Yoga.JSON as Json
import Yoga.JSON.Async as JsonAsync

data Action
    = Initialize
    | UpdateRegistration RegistrationInput.Output
    | Register Event
    | Navigate String MouseEvent

type State =
    { registration :: RegistrationInput.Input
    , otherError :: Boolean
    , submitting :: Boolean
    }

type Slot = H.Slot (Const Void) Void

render :: forall left.
    State -> H.ComponentHTML Action (registrationInput :: RegistrationInput.Slot) (Async left)
render { registration, otherError, submitting } =
    HH.form
    [ HP.class_ $ HH.ClassName "form"
    , HE.onSubmit  Register
    ]
    [ HH.h1 [ HP.class_ $ HH.ClassName "form-heading" ]
        [ HH.text "Create your "
        , HH.a
            [ HP.href "/"
            , HE.onClick $ Navigate "/"
            ]
            [ HH.text "TeamTavern" ]
        , HH.text " account"
        ]
    , registrationInput registration UpdateRegistration
    , HH.button
        [ HP.class_ $ HH.ClassName "form-submit-button"
        , HP.disabled $ registration.nickname == "" || registration.password == "" || submitting
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
            , HE.onClick $ Navigate "/signin"
            ]
            [ HH.text "Sign in." ]
        ]
    ]

sendRegisterRequest :: forall left. State -> Async left (Maybe State)
sendRegisterRequest state @ { registration: { nickname, password } } = Async.unify do
    response <- Fetch.fetch "/api/players"
        (  Fetch.method := POST
        <> Fetch.body := Json.writeJSON ({ nickname, password } :: RegisterPlayer.RequestContent)
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true })
                (\(error :: RegisterPlayer.BadContent) -> Just $ match
                    { registration: foldl (\state' -> match
                        { invalidNickname:
                            const $ state' { registration { nicknameError = true } }
                        , invalidPassword:
                            const $ state' { registration { passwordError = true } }
                        })
                        state
                    , nicknameTaken:
                        const $ state { registration { nicknameTaken = true } }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

handleAction :: forall slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Initialize = do
    H.liftEffect $ whenM hasPlayerIdCookie $ navigateReplace_ "/"
    setMeta "Create account | TeamTavern" "Create your TeamTavern account."
handleAction (UpdateRegistration registration) =
    H.modify_ _
        { registration
            { nickname = registration.nickname
            , password = registration.password
            }
        }
handleAction (Register event) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { registration
            { nicknameError  = false
            , passwordError  = false
            , nicknameTaken  = false
            }
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

component :: forall query input output left. H.Component query input output (Async left)
component = H.mkComponent
    { initialState: const
        { registration: RegistrationInput.emptyInput
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
register = HH.slot (Proxy :: _ "register") unit component unit absurd
