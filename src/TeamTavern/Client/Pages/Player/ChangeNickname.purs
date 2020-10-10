module TeamTavern.Client.Pages.Player.ChangeNickname
    (Message(..), Slot, changeNickname) where

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
import Data.String (trim)
import Data.Variant (SProxy(..), match)
import Effect.Class (class MonadEffect)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.CloseButton (closeButton)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Pages.Player.Types (Nickname)
import TeamTavern.Client.Script.Cookie (getPlayerInfo)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Player.ChangeNickname.SendResponse as ChangeNickname
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)
import Web.HTML.HTMLInputElement as HTMLInputElement

data Action
    = Init
    | NicknameInput String
    | Update LoadedState Event
    | Close

data Message = NicknameChanged Nickname | CloseClicked

type LoadedState =
    { originalNickname :: String
    , nickname :: String
    , nicknameError :: Boolean
    , nicknameTaken :: Boolean
    , otherError :: Boolean
    , submitting :: Boolean
    }

data State
    = Empty
    | Error
    | Loaded LoadedState

type Slot = H.Slot (Modal.Query Unit (Const Void)) (Modal.Message Message) Unit

render :: forall left children.
    State -> H.ComponentHTML Action children (Async left)
render Empty = HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] []
render Error = HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] []
render (Loaded loadedState @
    { nickname
    , nicknameError
    , nicknameTaken
    , otherError
    , submitting
    }) = HH.div [ HP.class_ $ HH.ClassName "single-form-container" ] $ pure $
    HH.form
    [ HP.class_ $ HH.ClassName "form"
    , HE.onSubmit $ Just <<< Update loadedState
    ]
    [ HH.h2 [ HP.class_ $ HH.ClassName "form-heading" ]
        [ HH.text "Change your nickname" ]
    , closeButton Close
    , HH.div [ HP.class_ $ HH.ClassName "input-groups" ]
        [ HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ HH.label
                [ HP.class_ $ HH.ClassName "input-label" ]
                [ HH.text "Nickname" ]
            , HH.input
                [ HP.ref $ H.RefLabel "nickname"
                , HP.class_ $ HH.ClassName "text-line-input"
                , HE.onValueInput $ Just <<< NicknameInput
                ]
            , HH.p
                [ HP.class_ $ inputErrorClass nicknameError ]
                [ HH.text
                    $ "The nickname can contain only alphanumeric characters "
                    <> "and cannot be more than 40 characters long." ]
            , HH.p
                [ HP.class_ $ inputErrorClass nicknameTaken ]
                [ HH.text
                    "This nickname is already taken, please pick another one." ]
            ]
        ]
    , HH.button
        [ HP.class_ $ ClassName "form-submit-button"
        , HP.disabled $ nickname == "" || submitting
        ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-edit button-icon" ] []
        , HH.text
            if submitting
            then "Changing nickname..."
            else "Change nickname"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

changeNickname' :: forall left. LoadedState -> Async left (Maybe LoadedState)
changeNickname' state @ { originalNickname, nickname } = Async.unify do
    response <- Fetch.fetch
        ("/api/players/by-nickname/" <> originalNickname <> "/nickname")
        (  Fetch.method := PUT
        <> Fetch.body := Json.writeJSON { nickname }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true})
                (\(error :: ChangeNickname.BadRequestContent) -> Just $ match
                    { invalidNickname: const $ state { nicknameError = true }
                    , nicknameTaken: const $ state { nicknameTaken = true }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

setStringValue
    :: forall state action slots message monad
    .  MonadEffect monad
    => String
    -> H.RefLabel
    -> H.HalogenM state action slots message monad Unit
setStringValue value label = do
    element <- H.getRef label
    let inputElement = element >>= HTMLInputElement.fromElement
    case inputElement of
        Nothing -> pure unit
        Just inputElement' -> H.liftEffect $
            HTMLInputElement.setValue value inputElement'

handleAction :: forall children left.
    Action -> H.HalogenM State Action children Message (Async left) Unit
handleAction Init = do
    playerInfo <- getPlayerInfo
    case playerInfo of
        Nothing -> H.liftEffect $ navigate_ "/"
        Just { nickname } -> do
            H.put $ Loaded
                { originalNickname: nickname
                , nickname
                , nicknameError: false
                , nicknameTaken: false
                , otherError: false
                , submitting: false
                }
            setStringValue nickname $ H.RefLabel "nickname"
handleAction (NicknameInput nickname) =
    H.modify_ case _ of
        Loaded state -> Loaded $ state { nickname = nickname }
        state -> state
handleAction (Update loadedState event) = do
    H.liftEffect $ preventDefault event
    let resetState = loadedState
            { nicknameError   = false
            , nicknameTaken   = false
            , otherError      = false
            , submitting      = true
            }
    H.put $ Loaded resetState
    newState <- H.lift $ changeNickname' resetState
    case newState of
        Nothing -> H.raise $ NicknameChanged $ trim resetState.nickname
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

changeNickname
    :: forall query children left
    .  (Modal.Message Message -> Maybe query)
    -> HH.ComponentHTML query (changeNickname :: Slot | children) (Async left)
changeNickname handleMessage = HH.slot
    (SProxy :: SProxy "changeNickname") unit
    (Modal.component component) unit handleMessage
