module TeamTavern.Client.Components.Password.ResetPassword where

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
import TeamTavern.Client.Script.Navigate (navigate, navigateReplace_)
import TeamTavern.Client.Script.QueryParams (getQueryParam)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Password.Reset.SendResponse as Reset
import Web.Event.Event as Event
import Web.Event.Internal.Types (Event)

data Action
    = Init
    | PasswordInput LoadedState String
    | TogglePasswordVisibility LoadedState
    | ResetPassword LoadedState Event

type LoadedState =
    { password :: String
    , passwordShown :: Boolean
    , invalidPassword :: Boolean
    , nonce :: String
    , invalidNonce :: Boolean
    , otherError :: Boolean
    }

data State = Empty | Loaded LoadedState

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots = (home :: NavigationAnchor.Slot Unit)

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div_ []
render (Loaded state @ { password, passwordShown, invalidPassword, nonce, invalidNonce, otherError }) =
    HH.form
    [ HP.class_ $ HH.ClassName "form", HE.onSubmit $ Just <<< ResetPassword state ]
    [ HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
        [ HH.text "Reset your "
        , navigationAnchor (SProxy :: SProxy "home")
            { path: "/", content: HH.text "TeamTavern" }
        , HH.text " password"
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "password" ]
            [ HH.text "New password" ]
        , HH.div [ HP.class_ $ HH.ClassName "password-input-container" ]
            [ HH.input
                [ HP.id_ "password"
                , HP.class_ $ HH.ClassName "password-input"
                , HP.type_
                    if passwordShown
                    then HP.InputText
                    else HP.InputPassword
                , HE.onValueInput $ Just <<< PasswordInput state
                ]
            , HH.button
                [ HP.class_ $ HH.ClassName "password-input-button"
                , HP.type_ HP.ButtonButton
                , HP.title
                    if passwordShown
                    then "Hide password"
                    else "Show password"
                , HE.onClick $ Just <<< const (TogglePasswordVisibility state)
                ]
                [ HH.i
                    [ HP.class_ $ HH.ClassName
                        if passwordShown
                        then "fas fa-eye-slash"
                        else "fas fa-eye"
                    ]
                    []
                ]
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass invalidPassword ]
            [ HH.text $ "The password mush have at least 8 characters." ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "form-submit-button"
        , HP.disabled $ password == ""
        ]
        [ HH.i [ HP.class_ $ H.ClassName "fas fa-key button-icon" ] []
        , HH.text "Reset password"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass invalidNonce ]
        [ HH.text $ "This password reset link has expired or is invalid. "
            <> "Please request another password reset link and try again."
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong. Please try again later." ]
    ]

sendPasswordResetRequest ::
    LoadedState -> (forall left. Async left (Maybe LoadedState))
sendPasswordResetRequest state @ { password, nonce } = Async.unify do
    response <- Fetch.fetch "/api/reset-password"
        (  Fetch.method := POST
        <> Fetch.body := Json.writeJSON { password, nonce }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true })
                (\(error :: Reset.BadRequestContent) -> Just $ match
                    { invalidPassword : const $ state { invalidPassword = true }
                    , invalidNonce: const $ state { invalidNonce = true }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    H.liftEffect $ whenM hasPlayerIdCookie $ navigateReplace_ "/"
    nonce <- H.liftEffect $ getQueryParam "nonce"
    case nonce of
        Nothing -> H.liftEffect $ navigateReplace_ "/"
        Just nonce' -> do
            state <- H.get
            case state of
                Empty -> H.put $ Loaded
                    { password: ""
                    , passwordShown: false
                    , invalidPassword: false
                    , nonce: nonce'
                    , invalidNonce: false
                    , otherError: false
                    }
                _ -> pure unit
    H.liftEffect do
        setMetaTitle "Reset your password | TeamTavern"
        setMetaDescription "Reset your TeamTavern password."
        setMetaUrl
handleAction (PasswordInput state password) =
    H.put $ Loaded $ state { password = password }
handleAction (TogglePasswordVisibility state) =
    H.put $ Loaded $ state { passwordShown = not state.passwordShown }
handleAction (ResetPassword state event) = do
    H.liftEffect $ Event.preventDefault event
    let state' = state
            { invalidPassword = false
            , invalidNonce    = false
            , otherError      = false
            }
    newState <- H.lift $ sendPasswordResetRequest state'
    case newState of
        Nothing -> H.liftEffect $
            navigate { password: state.password } "/reset-password-success"
        Just newState' -> H.put $ Loaded newState'

component :: forall query input output left.
    H.Component HH.HTML query input output (Async left)
component = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

resetPassword :: forall query children left.
    HH.ComponentHTML query (resetPassword :: Slot | children) (Async left)
resetPassword =
    HH.slot (SProxy :: SProxy "resetPassword") unit component unit absurd
