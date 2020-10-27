module TeamTavern.Client.Pages.Player.EditSettings where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Traversable (traverse)
import Data.Variant (SProxy(..))
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Script.Cookie (getPlayerInfo)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Snippets.ErrorClasses (otherErrorClass)
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLInputElement as HTMLInputElement

data Action
    = Init
    | Update LoadedState Event
    | Close

data Message = SettingsEdited | CloseClicked

type LoadedState =
    { nickname :: String
    , otherError :: Boolean
    , submitting :: Boolean
    }

data State
    = Empty
    | Error
    | Loaded LoadedState

type Slot = H.Slot (Const Void) (Modal.Output Message) Unit

render :: forall children left.
    State -> H.ComponentHTML Action children (Async left)
render Empty = HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] []
render Error = HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] []
render (Loaded loadedState @ { otherError, submitting }) =
    HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] $ pure $
    HH.form
    [ HP.class_ $ H.ClassName "form", HE.onSubmit $ Just <<< Update loadedState ]
    [ HH.div [ HP.class_ $ HH.ClassName "input-groups" ]
        [ HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.text "Message notifications" ]
            , HH.label
                [ HP.class_ $ HH.ClassName "checkbox-input-label"]
                [ HH.input
                    [ HP.ref $ H.RefLabel "notify"
                    , HP.class_ $ HH.ClassName "checkbox-input"
                    , HP.type_ HP.InputCheckbox
                    ]
                , HH.text "Send me an email when someone messages me."
                ]
            ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "form-submit-button"
        , HP.disabled submitting
        ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-edit button-icon" ] []
        , HH.text
            if submitting
            then "Editing settings..."
            else "Edit settings"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

loadSettings :: forall left. String -> Async left (Maybe { notify :: Boolean })
loadSettings nickname = Async.unify do
    response <-
        Fetch.fetch
        ("/api/players/by-nickname/" <> nickname <> "/settings")
        (Fetch.credentials := Fetch.Include)
        # lmap (const Nothing)
    content :: { notify :: Boolean } <-
        case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # lmap (const Nothing)
        _ -> Async.left Nothing
    pure $ Just content

editSettings' :: forall left.
    LoadedState -> { notify :: Boolean } -> Async left (Maybe LoadedState)
editSettings' state { notify } = Async.unify do
    response <-
        Fetch.fetch
        ("/api/players/by-nickname/" <> state.nickname <> "/settings")
        (  Fetch.method := PUT
        <> Fetch.body := Json.writeJSON { notify }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <-
        case FetchRes.status response of
        204 -> pure Nothing
        _ -> pure $ Just $ state { otherError = true }
    pure newState

getChecked :: forall state action slots message monad. MonadEffect monad =>
    H.RefLabel -> H.HalogenM state action slots message monad (Maybe Boolean)
getChecked label = do
    input <- H.getRef label
    input
        >>= HTMLInputElement.fromElement
        # traverse HTMLInputElement.checked
        # H.liftEffect

setChecked
    :: forall state action slots message monad
    .  MonadEffect monad
    => Boolean
    -> H.RefLabel
    -> H.HalogenM state action slots message monad Unit
setChecked checked label = do
    element <- H.getRef label
    let inputElement = element >>= HTMLInputElement.fromElement
    case inputElement of
        Nothing -> pure unit
        Just inputElement' -> H.liftEffect $
            HTMLInputElement.setChecked checked inputElement'

handleAction :: forall children left.
    Action -> H.HalogenM State Action children Message (Async left) Unit
handleAction Init = do
    playerInfo <- getPlayerInfo
    case playerInfo of
        Nothing -> navigate_ "/"
        Just { nickname } -> do
            settings <- H.lift $ loadSettings nickname
            case settings of
                Nothing -> H.put Error
                Just { notify } -> do
                    H.put $ Loaded
                        { nickname
                        , otherError: false
                        , submitting: false
                        }
                    setChecked notify $ H.RefLabel "notify"
handleAction (Update loadedState event) = do
    H.liftEffect $ preventDefault event
    notify <- getChecked $ H.RefLabel "notify"
    case notify of
        Nothing -> H.put $ Loaded $ loadedState
            { otherError = true
            , submitting = false
            }
        Just notify' -> do
            let resetState = loadedState
                    { otherError = false
                    , submitting = true
                    }
            H.put $ Loaded resetState
            newState <- H.lift $ editSettings' resetState { notify: notify' }
            case newState of
                Nothing -> H.raise SettingsEdited
                Just newState' -> H.put $ Loaded newState' { submitting = false }
handleAction Close = H.raise CloseClicked

component :: forall query input left.
    H.Component HH.HTML query input Message (Async left)
component = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

editSettings
    :: forall query children left
    .  (Modal.Output Message -> Maybe query)
    -> HH.ComponentHTML query (editSettings :: Slot | children) (Async left)
editSettings handleMessage = HH.slot
    (SProxy :: SProxy "editSettings") unit
    (Modal.component "Edit your account settings" component) unit handleMessage
