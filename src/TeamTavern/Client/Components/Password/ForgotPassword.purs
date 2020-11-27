module TeamTavern.Client.Components.Password.ForgotPassword (Slot, forgotPassword) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Variant (SProxy(..), match)
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
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Navigate (navigate, navigate_)
import TeamTavern.Client.Snippets.ErrorClasses (otherErrorClass)
import TeamTavern.Server.Password.Forgot.SendResponse as Forgot
import Web.Event.Event as Event
import Web.Event.Internal.Types (Event)

data Action
    = Init
    | EmailInput String
    | ResetPassword Event

type State =
    { email :: String
    , unknownEmail :: Boolean
    , otherError :: Boolean
    , submitting :: Boolean
    }

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots =
    ( home :: NavigationAnchor.Slot Unit
    , signinAnchor :: NavigationAnchor.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { email, unknownEmail, otherError, submitting } = HH.form
    [ HP.class_ $ HH.ClassName "form", HE.onSubmit $ Just <<< ResetPassword ]
    [ HH.h1 [ HP.class_ $ HH.ClassName "form-heading" ]
        [ HH.text "Reset your "
        , navigationAnchor (SProxy :: SProxy "home")
            { path: "/", content: HH.text "TeamTavern" }
        , HH.text " password"
        ]
    , HH.p [ HP.class_ $ HH.ClassName "form-subheading" ]
        [ HH.text $ "Enter your account email address "
            <> "and we will send you a link to reset your password."
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "email" ]
            [ HH.text "Email address" ]
        , HH.input
            [ HP.id_ "email"
            , HP.class_ $ HH.ClassName "text-line-input"
            , HE.onValueInput $ Just <<< EmailInput
            ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "form-submit-button"
        , HP.disabled $ email == "" || submitting
        ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-key button-icon" ] []
        , HH.text
            if submitting
            then "Sending password reset email..."
            else "Send password reset email"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass unknownEmail ]
        [ HH.text $ "Entered email address is unknown. "
            <> "Please check and try again."
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong. Please try again later." ]
    , HH.p
        [ HP.class_ $ HH.ClassName "form-bottom-text"]
        [ HH.text "Remembered your password? "
        , navigationAnchor (SProxy :: SProxy "signinAnchor")
            { path: "/signin", content: HH.text "Sign in." }
        ]
    ]

sendPasswordResetRequest :: forall left. State -> Async left (Maybe State)
sendPasswordResetRequest state @ { email } = Async.unify do
    response <- Fetch.fetch "/api/forgot-password"
        (  Fetch.method := POST
        <> Fetch.body := Json.writeJSON { email }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true })
                (\(error :: Forgot.BadRequestContent) -> Just $ match
                    { notFound: const $ state { unknownEmail = true }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    H.liftEffect $ whenM hasPlayerIdCookie $ navigate_ "/"
    H.liftEffect do
        setMetaTitle "Forgot password | TeamTavern"
        setMetaDescription "Request a password reset email."
        setMetaUrl
handleAction (EmailInput email) =
    H.modify_ (_ { email = email }) <#> const unit
handleAction (ResetPassword event) = do
    H.liftEffect $ Event.preventDefault event
    state <- H.gets (_
        { unknownEmail = false
        , otherError   = false
        , submitting   = true
        })
    H.put state
    newState <- H.lift $ sendPasswordResetRequest state
    case newState of
        Nothing -> H.liftEffect $
            navigate { email: state.email } "/reset-password-sent"
        Just newState' -> H.put newState' { submitting = false }
    pure unit

component :: forall query input output left.
    H.Component HH.HTML query input output (Async left)
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
        , initialize = Just Init
        }
    }

forgotPassword :: forall query children left.
    HH.ComponentHTML query (forgotPassword :: Slot | children) (Async left)
forgotPassword =
    HH.slot (SProxy :: SProxy "forgotPassword") unit component unit absurd
