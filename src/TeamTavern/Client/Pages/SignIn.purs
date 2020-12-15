module TeamTavern.Client.Pages.SignIn (Slot, signIn) where

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
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchor)
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Script.Cookie (hasPlayerIdCookie)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigateReplace_, navigate_)
import TeamTavern.Client.Snippets.ErrorClasses (otherErrorClass)
import TeamTavern.Server.Session.Start.ReadModel (StartDto)
import TeamTavern.Server.Session.Start.SendResponse as Start
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Action
    = Init
    | UpdateNickname String
    | UpdatePassword String
    | TogglePasswordVisibility
    | SignIn Event

type State =
    { nickname :: String
    , password :: String
    , passwordShown :: Boolean
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
    { nickname
    , password
    , passwordShown
    , noSessionStarted
    , otherError
    , submitting
    } = HH.form
    [ HP.class_ $ HH.ClassName "form", HE.onSubmit $ Just <<< SignIn ]
    [ HH.h1 [ HP.class_ $ HH.ClassName "form-heading" ]
        [ HH.text "Sign in to "
        , navigationAnchor (SProxy :: SProxy "home")
            { path: "/", content: HH.text "TeamTavern" }
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "nickname" ]
            [ HH.text "Nickname" ]
        , HH.input
            [ HP.id_ "nickname"
            , HP.class_ $ HH.ClassName "text-line-input"
            , HE.onValueInput $ Just <<< UpdateNickname
            ]
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "password" ]
            [ HH.text "Password" ]
        , HH.div [ HP.class_ $ HH.ClassName "password-input-container" ]
            [ HH.input
                [ HP.id_ "password"
                , HP.class_ $ HH.ClassName "password-input"
                , HP.type_
                    if passwordShown
                    then HP.InputText
                    else HP.InputPassword
                , HE.onValueInput $ Just <<< UpdatePassword
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
        , HP.disabled $ nickname == "" || password == "" || submitting
        ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-sign-in-alt button-icon" ] []
        , HH.text
            if submitting
            then "Signing in..."
            else "Sign in"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass noSessionStarted ]
        [ HH.text "Entered credentials don't appear to be valid. Please check and try again."
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
sendSignInRequest state @ { nickname, password } = Async.unify do
    response <- Fetch.fetch "/api/sessions"
        (  Fetch.method := POST
        <> Fetch.body := Json.writeJSON ({ nickname, password } :: StartDto)
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true })
                (\(error :: Start.BadRequestContent) -> Just $ match
                    { noSessionStarted:
                        const $ state { noSessionStarted = true }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Init = do
    H.liftEffect $ whenM hasPlayerIdCookie $ navigateReplace_ "/"
    setMeta "Sign in | TeamTavern" "Sign in to TeamTavern."
handleAction (UpdateNickname nickname) =
    H.modify_ (_ { nickname = nickname })
handleAction (UpdatePassword password) =
    H.modify_ (_ { password = password })
handleAction TogglePasswordVisibility =
    H.modify_ (\state -> state { passwordShown = not state.passwordShown })
handleAction (SignIn event) = do
    H.liftEffect $ preventDefault event
    state <- H.gets (_
        { noSessionStarted = false
        , otherError       = false
        , submitting       = true
        })
    H.put state
    newState <- H.lift $ sendSignInRequest state
    case newState of
        Nothing -> navigate_ "/"
        Just newState' -> H.put newState' { submitting = false }

component :: forall query input output left.
    H.Component HH.HTML query input output (Async left)
component = H.mkComponent
    { initialState: const
        { nickname: ""
        , password: ""
        , passwordShown: false
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
