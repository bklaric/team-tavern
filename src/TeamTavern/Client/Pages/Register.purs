module TeamTavern.Client.Pages.Register (register) where

import Prelude

import Async (Async)
import Async as Async
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Bifunctor (lmap)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (guard)
import Data.Variant (inj, match, onMatch)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Record as Record
import Record.Extra (pick)
import TeamTavern.Client.Components.Form (form, formError, otherFormError)
import TeamTavern.Client.Components.RegistrationInput (registrationInput)
import TeamTavern.Client.Components.RegistrationInput as RegistrationInput
import TeamTavern.Client.Components.RegistrationInputDiscord (registrationInputDiscord)
import TeamTavern.Client.Components.RegistrationInputDiscord as RegistrationInputDiscord
import TeamTavern.Client.Pages.Onboarding as Onboarding
import TeamTavern.Client.Pages.Preboarding (RegistrationMode(..))
import TeamTavern.Client.Script.Analytics (aliasNickname, identifyNickname, track_)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (hardNavigate, navigate, navigateWithEvent_)
import TeamTavern.Client.Script.QueryParams (getFragmentParam)
import TeamTavern.Client.Shared.Fetch (fetchBody)
import TeamTavern.Client.Shared.Slot (SimpleSlot)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Player.RegisterPlayer (RegisterPlayer)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)
import Web.UIEvent.MouseEvent (MouseEvent)
import Yoga.JSON (readJSON_, writeJSON)

data Action
    = Initialize
    | UpdateRegistrationEmail RegistrationInput.Output
    | UpdateRegistrationDiscord RegistrationInputDiscord.Output
    | Register Event
    | CreateWithEmail
    | CreateWithDiscord
    | Navigate String MouseEvent

type State =
    { registrationMode :: RegistrationMode
    , registrationEmail :: RegistrationInput.Input
    , registrationDiscord :: RegistrationInputDiscord.Input
    , accessToken :: Maybe String
    , discordError :: Boolean
    , otherError :: Boolean
    , submitting :: Boolean
    }

render :: ∀ left. State -> H.ComponentHTML Action _ (Async left)
render { registrationMode, registrationEmail, registrationDiscord, discordError, otherError, submitting } =
    form Register $
    [ HH.h1 [ HS.class_ "form-heading" ]
        [ HH.text "Create your "
        , HH.a
            [ HP.href "/"
            , HE.onClick $ Navigate "/"
            ]
            [ HH.text "TeamTavern" ]
        , HH.text " account"
        , HH.text case registrationMode of
            Email -> ""
            Discord -> " with Discord"
        ]
    ]
    <> case registrationMode of
        Email ->
            [ registrationInput registrationEmail UpdateRegistrationEmail
            , HH.button
                [ HS.class_ "primary-button"
                , HP.disabled submitting
                ]
                [ HH.i [ HS.class_ "fas fa-user-check button-icon" ] []
                , HH.text $
                    if submitting
                    then "Creating account..."
                    else "Create account"
                ]
            ]
        Discord ->
            [ registrationInputDiscord registrationDiscord UpdateRegistrationDiscord
            , HH.button
                [ HS.class_ "regular-button"
                , HP.disabled submitting
                ]
                [ HH.img
                    [ HS.class_ "button-icon"
                    , HP.style "height: 20px; vertical-align: top;"
                    , HP.src "https://coaching.healthygamer.gg/discord-logo-color.svg"
                    ]
                , HH.text "Create account with Discord"
                ]
            ]
    <> formError discordError "Yo, Discord is acting up."
    <> otherFormError otherError
    <>
    [ HH.div [HP.style "display: flex; align-items: center; margin: 28px 0;"]
        [ HH.hr [HP.style "flex: 1 1 auto;"]
        , HH.span [HP.style "padding: 0 10px"] [HH.text "Or"]
        , HH.hr [HP.style "flex: 1 1 auto;"]
        ]
    ]
    <> guard (registrationMode /= Email)
        [ HH.button
            [ HS.class_ "regular-button"
            , HP.type_ HP.ButtonButton
            , HE.onClick $ const CreateWithEmail
            ]
            [ HH.i [ HS.class_ "fas fa-envelope button-icon", HP.style "font-size: 20px;" ] []
            , HH.text "Create account with email"
            ]
        ]
    <> guard (registrationMode /= Discord)
        [ HH.button
            [ HS.class_ "regular-button"
            , HP.type_ HP.ButtonButton
            , HE.onClick $ const CreateWithDiscord
            ]
            [ HH.img
                [ HS.class_ "button-icon"
                , HP.style "height: 20px; vertical-align: top;"
                , HP.src "https://coaching.healthygamer.gg/discord-logo-color.svg"
                ]
            , HH.text "Create account with Discord"
            ]
        ]
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
sendRegisterRequest state = Async.unify do
    body <-
        case state.registrationMode, state.accessToken of
        Email, _ -> Async.right $ inj (Proxy :: _ "email") $ pick state.registrationEmail
        Discord, Just accessToken -> Async.right $ inj (Proxy :: _ "discord")
            {nickname: state.registrationDiscord.nickname, accessToken}
        Discord, Nothing -> do
            window >>= localStorage >>= setItem "register" (writeJSON state) # liftEffect
            hardNavigate $ "https://discord.com/api/oauth2/authorize"
                <> "?client_id=1068667687661740052"
                <> "&redirect_uri=https%3A%2F%2Flocalhost%2Fregister"
                <> "&response_type=token"
                <> "&scope=identify"
                <> "&prompt=none"
            Async.left $ Just state
    response' <- fetchBody (Proxy :: _ RegisterPlayer) body
        # lmap (const $ Just $ state { otherError = true })
    pure $ onMatch
        { noContent: const Nothing
        , badRequest: Just <<< match
            { registration: state # foldl \state' error' -> error' # match
                { email: const state' { registrationEmail { emailError = true } }
                , nickname: const
                    case state.registrationMode of
                    Email -> state' { registrationEmail { nicknameError = true } }
                    Discord -> state' { registrationDiscord { nicknameError = true } }
                , password: const state' { registrationEmail { passwordError = true } }
                }
            , emailTaken: const $ state { registrationEmail { emailTaken = true } }
            , nicknameTaken: const
                case state.registrationMode of
                Email -> state { registrationEmail { nicknameTaken = true } }
                Discord -> state { registrationDiscord { nicknameTaken = true } }
            }
        }
        (const $ Just $ state { otherError = true })
        response'

tryToRegister :: forall slots output left.
    State -> H.HalogenM State Action slots output (Async left) Unit
tryToRegister state = do
    H.put state
    newStateMaybe <- H.lift $ sendRegisterRequest state
    case newStateMaybe of
        Nothing -> do
            aliasNickname
            identifyNickname
            track_ "Register"
            navigate Onboarding.emptyInput "/onboarding/start"
        Just newState -> H.put newState {submitting = false}

handleAction :: ∀ slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Initialize = do
    setMeta "Create account | TeamTavern" "Create your TeamTavern account."
    -- Check if we came back from Discord sign in page.
    stateMaybe <- runMaybeT do
        accessToken <- MaybeT $ getFragmentParam "access_token"
        stateJson <- window >>= localStorage >>= getItem "register" # liftEffect # MaybeT
        state <- (readJSON_ :: _ -> _ State) stateJson # pure # MaybeT
        pure state {accessToken = Just accessToken}
    stateMaybe # maybe (pure unit) tryToRegister
handleAction (UpdateRegistrationEmail registration) =
    H.modify_ \state -> state { registrationEmail = Record.merge registration state.registrationEmail }
handleAction (UpdateRegistrationDiscord registration) =
    H.modify_ \state -> state { registrationDiscord = Record.merge registration state.registrationDiscord }
handleAction (Register event) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { registrationEmail
            { emailError = false
            , nicknameError  = false
            , passwordError  = false
            , emailTaken = false
            , nicknameTaken  = false
            }
        , registrationDiscord
            { nicknameError = false
            , nicknameTaken = false
            }
        , otherError = false
        , submitting = true
        })
    tryToRegister state
handleAction (Navigate url event) =
    navigateWithEvent_ url event
handleAction CreateWithEmail =
    H.modify_ _ {registrationMode = Email}
handleAction CreateWithDiscord =
    H.modify_ _ {registrationMode = Discord}

component :: ∀ query input output left. H.Component query input output (Async left)
component = H.mkComponent
    { initialState: const
        { registrationMode: Email
        , registrationEmail: RegistrationInput.emptyInput
        , registrationDiscord: RegistrationInputDiscord.emptyInput
        , accessToken: Nothing
        , discordError: false
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
