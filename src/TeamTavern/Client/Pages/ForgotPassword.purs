module TeamTavern.Client.Pages.ForgotPassword (forgotPassword) where

import Prelude

import Async (Async)
import Async as Async
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..), isNothing)
import Data.Variant (match, onMatch)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Form (form, formError, otherFormError)
import TeamTavern.Client.Components.Input (inputGroup, inputLabel_, requiredTextLineInput)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Script.Analytics (track_)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigate)
import TeamTavern.Client.Shared.Fetch (fetchBody)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Password.ForgotPassword (ForgotPassword)
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.Event.Internal.Types (Event)

data Action
    = Initialize
    | UpdateEmail String
    | ResetPassword Event

type State =
    { email :: String
    , unknownEmail :: Boolean
    , otherError :: Boolean
    , submitting :: Boolean
    }

render :: forall left. State -> H.ComponentHTML Action _ (Async left)
render { email, unknownEmail, otherError, submitting } =
    form ResetPassword $
    [ HH.h1 [ HS.class_ "form-heading" ]
        [ HH.text "Reset your "
        , navigationAnchor (Proxy :: _ "home")
            { path: "/", content: HH.text "TeamTavern" }
        , HH.text " password"
        ]
    , HH.p [ HS.class_ "form-subheading" ]
        [ HH.text $ "Enter your account email address "
            <> "and you will receive a link to reset your password."
        ]
    , inputGroup
        [ inputLabel_ "Email"
        , requiredTextLineInput email UpdateEmail
        ]
    , HH.button
        [ HS.class_ "primary-button"
        , HP.disabled $ email == "" || submitting
        ]
        [ HH.i [ HS.class_ "fas fa-key button-icon" ] []
        , HH.text
            if submitting
            then "Sending password reset email..."
            else "Send password reset email"
        ]
    ]
    <> formError unknownEmail "No account exists with this email."
    <> otherFormError otherError
    <>
    [ HH.p
        [ HS.class_ "form-bottom-text"]
        [ HH.text "Remembered your password? "
        , navigationAnchor (Proxy :: _ "signinAnchor")
            { path: "/signin", content: HH.text "Sign in." }
        ]
    ]

sendPasswordResetRequest :: forall left. State -> Async left (Maybe State)
sendPasswordResetRequest state @ {email} = Async.unify do
    response <- fetchBody (Proxy :: _ ForgotPassword) {email}
        # lmap (const $ Just $ state {otherError = true})
    nextState <- pure $ onMatch
        { noContent: const Nothing
        , notFound: const $ Just $ state {unknownEmail = true}
        }
        (const $ Just $ state {otherError = true})
        response
    when (isNothing nextState) $ track_ "Password forgot"
    pure nextState

handleAction :: forall slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Initialize =
    setMeta "Forgot password | TeamTavern" "Request a password reset email."
handleAction (UpdateEmail email) =
    H.modify_ _ {email = email}
handleAction (ResetPassword event) = do
    H.liftEffect $ Event.preventDefault event
    state <- H.modify _
        { unknownEmail = false
        , otherError   = false
        , submitting   = true
        }
    newState <- H.lift $ sendPasswordResetRequest state
    case newState of
        Nothing -> H.liftEffect $
            navigate { email: state.email } "/reset-password-sent"
        Just newState' -> H.put newState' { submitting = false }

component :: forall query input output left.
    H.Component query input output (Async left)
component = H.mkComponent
    { initialState: const
        { email: ""
        , unknownEmail: false
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

forgotPassword :: forall query left. HH.ComponentHTML query _ (Async left)
forgotPassword =
    HH.slot (Proxy :: _ "forgotPassword") unit component unit absurd
