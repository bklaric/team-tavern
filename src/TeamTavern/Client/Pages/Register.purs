module TeamTavern.Client.Pages.Register (register) where

import Prelude

import Async (Async)
import Async as Async
import Data.Bifunctor (lmap)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Variant (match, onMatch)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record
import TeamTavern.Client.Components.Form (form, otherFormError)
import TeamTavern.Client.Components.RegistrationInput (registrationInput)
import TeamTavern.Client.Components.RegistrationInput as RegistrationInput
import TeamTavern.Client.Pages.Onboarding as Onboarding
import TeamTavern.Client.Script.Analytics (aliasNickname, identifyNickname, track_)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigate, navigateWithEvent_)
import TeamTavern.Client.Shared.Fetch (fetchBody)
import TeamTavern.Client.Shared.Slot (SimpleSlot)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Player.RegisterPlayer (RegisterPlayer)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)
import Web.UIEvent.MouseEvent (MouseEvent)

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

render :: ∀ left. State -> H.ComponentHTML Action _ (Async left)
render { registration, otherError, submitting } =
    form Register $
    [ HH.h1 [ HS.class_ "form-heading" ]
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
        [ HS.class_ "form-submit-button"
        , HP.disabled
            $ registration.email == ""
            || registration.nickname == ""
            || registration.password == ""
            || submitting
        ]
        [ HH.i [ HS.class_ "fas fa-user-check button-icon" ] []
        , HH.text $
            if submitting
            then "Creating account..."
            else "Create account"
        ]
    ]
    <> otherFormError otherError
    <>
    [ HH.p
        [ HS.class_ "form-bottom-text" ]
        [ HH.text "Already have an account? "
        , HH.a
            [ HP.href "/signin"
            , HE.onClick $ Navigate "/signin"
            ]
            [ HH.text "Sign in." ]
        ]
    ]

sendRegisterRequest :: ∀ left. State -> Async left (Maybe State)
sendRegisterRequest state @ { registration: { email, nickname, password } } = Async.unify do
    response' <- fetchBody (Proxy :: _ RegisterPlayer) { email, nickname, password }
        # lmap (const $ Just $ state { otherError = true })
    pure $ onMatch
        { noContent: const Nothing
        , badRequest: Just <<< match
            { registration: foldl (\state' -> match
                { email: const $ state' { registration { emailError = true } }
                , nickname: const $ state' { registration { nicknameError = true } }
                , password: const $ state' { registration { passwordError = true } }
                })
                state
            , emailTaken: const $ state { registration { emailTaken = true } }
            , nicknameTaken: const $ state { registration { nicknameTaken = true } }
            }
        }
        (const $ Just $ state { otherError = true })
        response'

handleAction :: ∀ slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Initialize =
    setMeta "Create account | TeamTavern" "Create your TeamTavern account."
handleAction (UpdateRegistration registration) =
    H.modify_ \state -> state { registration = Record.merge registration state.registration }
handleAction (Register event) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { registration
            { emailError = false
            , nicknameError  = false
            , passwordError  = false
            , emailTaken = false
            , nicknameTaken  = false
            }
        , otherError = false
        , submitting = true
        })
    H.put state
    newState <- H.lift $ sendRegisterRequest state
    case newState of
        Nothing -> do
            aliasNickname
            identifyNickname
            track_ "Register"
            navigate Onboarding.emptyInput "/onboarding/start"
        Just newState' -> H.put newState' { submitting = false }
handleAction (Navigate url event) =
    navigateWithEvent_ url event

component :: ∀ query input output left. H.Component query input output (Async left)
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

register :: ∀ query children left.
    HH.ComponentHTML query (register :: SimpleSlot | children) (Async left)
register = HH.slot (Proxy :: _ "register") unit component unit absurd
