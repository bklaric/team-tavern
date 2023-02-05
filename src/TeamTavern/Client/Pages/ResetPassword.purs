module TeamTavern.Client.Pages.ResetPassword where

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
import TeamTavern.Client.Components.Input (inputError, inputGroup, inputLabel_)
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.PasswordInput (passwordInput)
import TeamTavern.Client.Script.Analytics (track_)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigateReplace_, navigate_)
import TeamTavern.Client.Script.QueryParams (getQueryParam)
import TeamTavern.Client.Shared.Fetch (fetchBody)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Password.ResetPassword (ResetPassword)
import Type.Proxy (Proxy(..))
import Web.Event.Event as Event
import Web.Event.Internal.Types (Event)

data Action
    = Initialize
    | UpdatePassword LoadedState String
    | TogglePasswordVisibility LoadedState
    | ResetPassword LoadedState Event

type LoadedState =
    { password :: String
    , passwordShown :: Boolean
    , passwordError :: Boolean
    , nonce :: String
    , nonceError :: Boolean
    , otherError :: Boolean
    , submitting :: Boolean
    }

data State = Empty | Loaded LoadedState

render :: forall left. State -> H.ComponentHTML Action _ (Async left)
render Empty = HH.div_ []
render (Loaded state @
    { password
    , passwordShown
    , passwordError
    , nonceError
    , otherError
    , submitting
    }) =
    form (ResetPassword state) $
    [ HH.h1 [ HS.class_ "form-heading" ]
        [ HH.text "Reset your "
        , navigationAnchor (Proxy :: _ "home")
            { path: "/", content: HH.text "TeamTavern" }
        , HH.text " password"
        ]
    , inputGroup $
        [ inputLabel_ "New password"
        , passwordInput password passwordShown (UpdatePassword state) (TogglePasswordVisibility state)
        ]
        <> inputError passwordError "The password must have at least 8 characters."
    , HH.button
        [ HS.class_ "primary-button"
        , HP.disabled $ password == "" || submitting
        ]
        [ HH.i [ HS.class_ "fas fa-key button-icon" ] []
        , HH.text
            if submitting
            then "Reseting password..."
            else "Reset password"
        ]
    ]
    <> formError nonceError ("This password reset link has expired or is invalid. "
        <> "Please request another password reset link and try again.")
    <> otherFormError otherError

sendPasswordResetRequest ::
    LoadedState -> (forall left. Async left (Maybe LoadedState))
sendPasswordResetRequest state @ {password, nonce} = Async.unify do
    response <- fetchBody (Proxy :: _ ResetPassword) {password, nonce}
        # lmap (const $ Just $ state { otherError = true })
    nextState <- pure $ onMatch
        { noContent: const Nothing
        , badRequest: match
            {password: const $ Just $ state {passwordError = true}}
        , notFound: const $ Just $ state {nonceError = true}
        }
        (const $ Just $ state {otherError = true})
        response
    when (isNothing nextState) $ track_ "Password reset"
    pure nextState

handleAction :: forall slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Initialize = do
    nonce <- getQueryParam "nonce"
    case nonce of
        Nothing -> navigateReplace_ "/"
        Just nonce' -> H.put $ Loaded
            { password: ""
            , passwordShown: false
            , passwordError: false
            , nonce: nonce'
            , nonceError: false
            , otherError: false
            , submitting: false
            }
    setMeta  "Reset your password | TeamTavern" "Reset your TeamTavern password."
handleAction (UpdatePassword state password) =
    H.put $ Loaded $ state { password = password }
handleAction (TogglePasswordVisibility state) =
    H.put $ Loaded $ state { passwordShown = not state.passwordShown }
handleAction (ResetPassword state event) = do
    H.liftEffect $ Event.preventDefault event
    let state' = state
            { passwordError = false
            , nonceError    = false
            , otherError    = false
            , submitting    = true
            }
    H.put $ Loaded state'
    newState <- H.lift $ sendPasswordResetRequest state'
    case newState of
        Nothing -> navigate_ "/reset-password-success"
        Just newState' -> H.put $ Loaded newState' { submitting = false }

component :: forall query input output left.
    H.Component query input output (Async left)
component = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

resetPassword :: forall query left. HH.ComponentHTML query _ (Async left)
resetPassword = HH.slot (Proxy :: _ "resetPassword") unit component unit absurd
