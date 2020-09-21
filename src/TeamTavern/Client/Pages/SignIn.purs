module TeamTavern.Client.Pages.SignIn (Slot, signIn) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Options ((:=))
import Data.Variant (SProxy(..), match)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor, navigationAnchorClassed)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Script.Cookie (hasPlayerIdCookie)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Script.QueryParams (getQueryParam)
import TeamTavern.Client.Snippets.ErrorClasses (otherErrorClass)
import TeamTavern.Server.Session.Start.SendResponse as Start
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Action
    = Init
    | NicknameOrEmailInput String
    | PasswordInput String
    | TogglePasswordVisibility
    | SignIn Event

type State =
    { nicknameOrEmail :: String
    , password :: String
    , passwordShown :: Boolean
    , nonce :: Maybe String
    , unconfirmedEmail :: Boolean
    , nothingConfirmed :: Boolean
    , noSessionStarted :: Boolean
    , otherError :: Boolean
    , submitting :: Boolean
    }

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( home :: NavigationAnchor.Slot Unit
    , forgotPasswordAnchor :: NavigationAnchor.Slot Unit
    , registerAnchor :: NavigationAnchor.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render
    { nicknameOrEmail
    , password
    , passwordShown
    , nonce
    , unconfirmedEmail
    , nothingConfirmed
    , noSessionStarted
    , otherError
    , submitting
    } = HH.form
    [ HP.class_ $ HH.ClassName "form", HE.onSubmit $ Just <<< SignIn ]
    [ HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
        [ HH.text "Sign in to "
        , navigationAnchor (SProxy :: SProxy "home")
            { path: "/", content: HH.text "TeamTavern" }
        , HH.text $ maybe "" (const " to confirm your email address") nonce
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "nicknameOrEmail" ]
            [ HH.text "Nickname or email address" ]
        , HH.input
            [ HP.id_ "nicknameOrEmail"
            , HP.class_ $ HH.ClassName "text-line-input"
            , HE.onValueInput $ Just <<< NicknameOrEmailInput
            ]
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "password" ]
            [ HH.text "Password"
            , navigationAnchorClassed (SProxy :: SProxy "forgotPasswordAnchor")
                { class_: "forgot-password"
                , path: "/forgot-password"
                , content: HH.text "Forgot password?"
                }
            ]
        , HH.div [ HP.class_ $ HH.ClassName "password-input-container" ]
            [ HH.input
                [ HP.id_ "password"
                , HP.class_ $ HH.ClassName "password-input"
                , HP.type_
                    if passwordShown
                    then HP.InputText
                    else HP.InputPassword
                , HE.onValueInput $ Just <<< PasswordInput
                ]
            , HH.button
                [ HP.class_ $ HH.ClassName "password-input-button"
                , HP.type_ HP.ButtonButton
                , HP.title
                    if passwordShown
                    then "Hide password"
                    else "Show password"
                , HE.onClick $ Just <<< const TogglePasswordVisibility
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
        ]
    , HH.button
        [ HP.class_ $ HH.ClassName "form-submit-button"
        , HP.disabled $ nicknameOrEmail == "" || password == "" || submitting
        ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-sign-in-alt button-icon" ] []
        , HH.text
            if submitting
            then "Signing in..."
            else "Sign in"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass unconfirmedEmail ]
        [ HH.text "Please confirm your email address before signing in."
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass nothingConfirmed ]
        [ HH.text
            $  "Something went wrong with confirming your email address. "
            <> "Please try again later or contact the administrator."
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass noSessionStarted ]
        [ HH.text
            $  "Entered credentials don't appear to be valid. "
            <> "Please check and try again."
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong. Please try again later." ]
    , HH.p
        [ HP.class_ $ HH.ClassName "form-bottom-text"]
        [ HH.text "New to TeamTavern? "
        , navigationAnchor (SProxy :: SProxy "registerAnchor")
            { path: "/register", content: HH.text "Create an account." }
        ]
    ]

sendSignInRequest :: forall left. State -> Async left (Maybe State)
sendSignInRequest state @ { nicknameOrEmail, password, nonce } = Async.unify do
    response <- Fetch.fetch "/api/sessions"
        (  Fetch.method := POST
        <> Fetch.body := Json.writeJSON { nicknameOrEmail, password, nonce }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true })
                (\(error :: Start.BadRequestContent) -> Just $ match
                    { unconfirmedEmail:
                        const $ state { unconfirmedEmail = true }
                    , nothingConfirmed:
                        const $ state { nothingConfirmed = true }
                    , noSessionStarted:
                        const $ state { noSessionStarted = true }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    H.liftEffect $ whenM hasPlayerIdCookie $ navigate_ "/"
    nonce <- H.liftEffect $ getQueryParam "nonce"
    H.modify_ (_ { nonce = nonce })
    H.liftEffect do
        setMetaTitle "Sign in | TeamTavern"
        setMetaDescription "Sign in to TeamTavern."
        setMetaUrl
handleAction (NicknameOrEmailInput nicknameOrEmail) =
    H.modify_ (_ { nicknameOrEmail = nicknameOrEmail })
handleAction (PasswordInput password) =
    H.modify_ (_ { password = password })
handleAction TogglePasswordVisibility =
    H.modify_ (\state -> state { passwordShown = not state.passwordShown })
handleAction (SignIn event) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { unconfirmedEmail = false
        , nothingConfirmed = false
        , noSessionStarted = false
        , otherError       = false
        , submitting       = true
        })
    H.put state
    newState <- H.lift $ sendSignInRequest state
    case newState of
        Nothing -> H.liftEffect $ navigate_ "/"
        Just newState' -> H.put newState' { submitting = false }

component :: forall query input output left.
    H.Component HH.HTML query input output (Async left)
component = H.mkComponent
    { initialState: const
        { nicknameOrEmail: ""
        , password: ""
        , passwordShown: false
        , nonce: Nothing
        , unconfirmedEmail: false
        , nothingConfirmed: false
        , noSessionStarted: false
        , otherError: false
        , submitting: false
        }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

signIn :: forall query children left.
    HH.ComponentHTML query (signIn :: Slot Unit | children) (Async left)
signIn = HH.slot (SProxy :: SProxy "signIn") unit component unit absurd
