module TeamTavern.Client.Pages.SignIn (signIn) where

import Prelude

import Async (Async)
import Async as Async
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.Variant (match, onMatch)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Form (form, otherFormError)
import TeamTavern.Client.Components.Input (inputError, inputGroup, inputLabel_, requiredTextLineInput)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor, navigationAnchorClassed)
import TeamTavern.Client.Components.PasswordInput (passwordInput)
import TeamTavern.Client.Script.Analytics (registerSignedIn)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Shared.Fetch (fetchBody)
import TeamTavern.Client.Shared.Slot (SimpleSlot)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Session.StartSession (StartSession)
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Action
    = Init
    | UpdateEmailOrNickname String
    | UpdatePassword String
    | TogglePasswordVisibility
    | SignIn Event

type State =
    { emailOrNickname :: String
    , password :: String
    , passwordShown :: Boolean
    , unknownPlayer :: Boolean
    , wrongPassword :: Boolean
    , otherError :: Boolean
    , submitting :: Boolean
    }

render :: ∀ left. State -> H.ComponentHTML Action _ (Async left)
render
    { emailOrNickname
    , password
    , passwordShown
    , unknownPlayer
    , wrongPassword
    , otherError
    , submitting
    } = form SignIn $
    [ HH.h1 [ HS.class_ "form-heading" ]
        [ HH.text "Sign in to "
        , navigationAnchor (Proxy :: _ "home")
            { path: "/", content: HH.text "TeamTavern" }
        ]
    , inputGroup $
        [ inputLabel_ "Email or nickname"
        , requiredTextLineInput emailOrNickname UpdateEmailOrNickname
        ]
        <> inputError unknownPlayer "No account exists with this email or nickname."
    , inputGroup $
        [ HH.label
            [ HS.class_ "input-label" ]
            [ HH.text "Password"
            , navigationAnchorClassed (Proxy :: _ "forgotPasswordAnchor")
                { class_: "forgot-password"
                , path: "/forgot-password"
                , content: HH.text "Forgot password?"
                }
            ]
        , passwordInput password passwordShown UpdatePassword TogglePasswordVisibility
        ]
        <> inputError wrongPassword "Entered password is incorrect."
    , HH.button
        [ HS.class_ "form-submit-button"
        , HP.disabled $ emailOrNickname == "" || password == "" || submitting
        ]
        [ HH.i [ HS.class_ "fas fa-sign-in-alt button-icon" ] []
        , HH.text
            if submitting
            then "Signing in..."
            else "Sign in"
        ]
    ]
    <> otherFormError otherError
    <>
    [ HH.p
        [ HS.class_ "form-bottom-text"]
        [ HH.text "New to TeamTavern? "
        , navigationAnchor (Proxy :: _ "registerAnchor")
            { path: "/register", content: HH.text "Create an account." }
        ]
    ]

sendSignInRequest :: ∀ left. State -> Async left (Maybe State)
sendSignInRequest state @ { emailOrNickname, password } = Async.unify do
    response <- fetchBody (Proxy :: _ StartSession) { emailOrNickname, password }
        # lmap (const $ Just $ state { otherError = true })
    pure $ onMatch
        { noContent: const Nothing
        , badRequest: \error -> Just $ match
            { unknownPlayer: const $ state { unknownPlayer = true }
            , wrongPassword: const $ state { wrongPassword = true }
            }
            error
        }
        (const $ Just state { otherError = true })
        response

handleAction :: ∀ slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Init =
    setMeta "Sign in | TeamTavern" "Sign in to TeamTavern."
handleAction (UpdateEmailOrNickname emailOrNickname) =
    H.modify_ (_ { emailOrNickname = emailOrNickname })
handleAction (UpdatePassword password) =
    H.modify_ (_ { password = password })
handleAction TogglePasswordVisibility =
    H.modify_ (\state -> state { passwordShown = not state.passwordShown })
handleAction (SignIn event) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { unknownPlayer = false
        , wrongPassword = false
        , otherError    = false
        , submitting    = true
        })
    H.put state
    newState <- H.lift $ sendSignInRequest state
    case newState of
        Nothing -> do
            registerSignedIn
            navigate_ "/"
        Just newState' -> H.put newState' { submitting = false }

component :: ∀ query input output left.
    H.Component query input output (Async left)
component = H.mkComponent
    { initialState: const
        { emailOrNickname: ""
        , password: ""
        , passwordShown: false
        , unknownPlayer: false
        , wrongPassword: false
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

signIn :: ∀ query children left.
    HH.ComponentHTML query (signIn :: SimpleSlot | children) (Async left)
signIn = HH.slot (Proxy :: _ "signIn") unit component unit absurd
