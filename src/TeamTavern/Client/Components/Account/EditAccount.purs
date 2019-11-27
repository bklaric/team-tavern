module TeamTavern.Client.Components.Account.EditAccount
    (Message(..), Slot, editAccount) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.Foldable (foldl, intercalate)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.String (trim)
import Data.Variant (SProxy(..), match)
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
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass, otherErrorClass)
import TeamTavern.Server.Player.Update.SendResponse as Update
import TeamTavern.Server.Player.ViewAccount.SendResponse as ViewAccount
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Action
    = Init
    | NicknameInput String
    | AboutInput String
    | NotifyInput Boolean
    | Update Event

data Message = AccountUpdated String

type LoadedState =
    { originalNickname :: String
    , nickname :: String
    , about :: String
    , notify :: Boolean
    , nicknameError :: Boolean
    , aboutError :: Boolean
    , nicknameTaken :: Boolean
    , otherError :: Boolean
    }

data State
    = Empty
    | Error
    | Loaded LoadedState

type Slot = H.Slot (Modal.Query Unit (Const Void)) (Modal.Message Message) Unit

render :: forall slots. State -> HH.HTML slots Action
render Empty = HH.div [ HP.class_ $ HH.ClassName "wide-single-form-container" ] []
render Error = HH.div [ HP.class_ $ HH.ClassName "wide-single-form-container" ]
    [ HH.p_ [ HH.text "There was an error while loading account details. Please try again later." ] ]
render (Loaded
    { originalNickname
    , nickname
    , about
    , notify
    , nicknameError
    , aboutError
    , nicknameTaken
    , otherError
    }) = HH.div [ HP.class_ $ HH.ClassName "wide-single-form-container" ] $ pure $ HH.form
    [ HP.class_ $ H.ClassName "form", HE.onSubmit $ Just <<< Update ]
    [ HH.h2  [ HP.class_ $ HH.ClassName "form-heading" ]
        [ HH.text "Edit your account" ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "nickname" ]
            [ HH.text "Nickname" ]
        , HH.input
            [ HP.id_ "nickname"
            , HP.class_ $ HH.ClassName "text-line-input"
            , HE.onValueInput $ Just <<< NicknameInput
            , HP.value nickname
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass nicknameError ]
            [ HH.text
                $ "The nickname can contain only alphanumeric characters and "
                <> "cannot be more than 40 characters long." ]
        , HH.p
            [ HP.class_ $ inputErrorClass nicknameTaken ]
            [ HH.text
                "This nickname is already taken, please pick another one." ]
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label", HP.for "notify" ]
            [ HH.input
                [ HP.id_ "notify"
                , HP.class_ $ HH.ClassName "checkbox-input"
                , HP.type_ HP.InputCheckbox
                , HE.onChecked $ Just <<< NotifyInput
                , HP.checked notify
                ]
            , HH.text "Send an email when someone messages you"
            ]
        ]
    -- , HH.div_
    --     [ HH.label
    --         [ HP.for "about" ]
    --         [ HH.text "About" ]
    --     , HH.textarea
    --         [ HP.id_ "about"
    --         , HE.onValueInput $ Just <<< AboutInput
    --         , HP.value about
    --         ]
    --     , HH.p
    --         [ HP.class_ $ inputErrorClass aboutError ]
    --         [ HH.text
    --             "The about entry cannot be more than 2000 characters long." ]
    --     ]
    , HH.button
        [ HP.class_ $ ClassName "form-submit-button"
        , HP.disabled $ nickname == ""
        ]
        [ HH.i [ HP.class_ $ HH.ClassName "fas fa-edit button-icon" ] []
        , HH.text "Edit account"
        ]
    , HH.p
        [ HP.class_ $ otherErrorClass otherError ]
        [ HH.text "Something unexpected went wrong! Please try again later." ]
    ]

loadAccount :: forall left. String -> Async left State
loadAccount nickname = Async.unify do
    response <- Fetch.fetch
        ("/api/players/by-nickname/" <> nickname <> "/account")
        (Fetch.credentials := Fetch.Include)
        # lmap (const Error)
    content :: ViewAccount.OkContent <- case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # lmap (const Error)
        _ -> Async.left Error
    pure $ Loaded
        { originalNickname: content.nickname
        , nickname: content.nickname
        , about: intercalate "\n\n" content.about
        , notify: content.notify
        , nicknameError: false
        , aboutError: false
        , nicknameTaken: false
        , otherError: false
        }

updateAccount :: forall left. LoadedState -> Async left (Maybe LoadedState)
updateAccount state @ { originalNickname, nickname, about, notify } = Async.unify do
    response <- Fetch.fetch ("/api/players/by-nickname/" <> nickname)
        (  Fetch.method := PUT
        <> Fetch.body := Json.writeJSON { nickname, about, notify }
        <> Fetch.credentials := Fetch.Include
        )
        # lmap (const $ Just $ state { otherError = true })
    newState <- case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true})
                (\(error :: Update.BadRequestContent) -> Just $ match
                    { invalidIdentifiers: foldl (\state' -> match
                        { invalidNickname:
                            const $ state' { nicknameError = true }
                        , invalidAbout:
                            const $ state' { aboutError = true }
                        })
                        state
                    , nicknameTaken: const $ state { nicknameTaken = true }
                    }
                    error)
        _ -> pure $ Just $ state { otherError = true }
    pure newState

handleAction :: forall slots left.
    Action -> H.HalogenM State Action slots Message (Async left) Unit
handleAction Init = do
    playerInfo <- H.liftEffect getPlayerInfo
    case playerInfo of
        Nothing -> H.liftEffect $ navigate_ "/"
        Just { nickname } -> do
            state <- H.lift $ loadAccount nickname
            H.put state
handleAction (NicknameInput nickname) =
    H.modify_ case _ of
        Loaded state -> Loaded $ state { nickname = nickname }
        state -> state
handleAction (AboutInput about) = do
    H.modify_ case _ of
        Loaded state -> Loaded $ state { about = about }
        state -> state
handleAction (NotifyInput notify) = do
    H.modify_ case _ of
        Loaded state -> Loaded $ state { notify = notify }
        state -> state
handleAction (Update event) = do
    H.liftEffect $ preventDefault event
    state <- H.get
    case state of
        Loaded loadedState -> do
            let resetState = loadedState
                    { nicknameError = false
                    , aboutError    = false
                    , nicknameTaken = false
                    , otherError    = false
                    }
            newState <- H.lift $ updateAccount resetState
            case newState of
                Nothing -> H.raise $ AccountUpdated $ trim loadedState.nickname
                Just newState' -> H.put $ Loaded newState'
        otherState -> pure unit

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

editAccount
    :: forall query children left
    .  (Modal.Message Message -> Maybe query)
    -> HH.ComponentHTML query (editAccount :: Slot | children) (Async left)
editAccount handleMessage = HH.slot
    (SProxy :: SProxy "editAccount") unit
    (Modal.component component) unit handleMessage
