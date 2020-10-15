module TeamTavern.Client.Pages.Conversation (Slot, conversation) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array as Array
import Data.Bifunctor (bimap, lmap)
import Data.Const (Const)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Symbol (SProxy(..))
import Data.Variant (match)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Input (inputError, inputGroup, textInput)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass)
import TeamTavern.Server.Conversation.Start.SendResponse as Conversation
import TeamTavern.Server.Conversation.View.SendResponse as View
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

type Nickname = String

type Path = String

playerPath :: Nickname -> Path
playerPath nickname = "/players/" <> nickname

data Action
    = Init
    | UpdateMessage String
    | SendMessage MouseEvent
    | Navigate Path MouseEvent

data State
    = Empty Nickname
    | Error
    | Conversation
        { nickname :: Nickname
        , conversation :: View.OkContent
        , message ::  String
        , messageError :: Boolean
        , otherError :: Boolean
        }

type Slot = H.Slot (Const Void) Void Unit

render (Empty _) = HH.div_ []
render Error = HH.p_ [ HH.text "There has been an error loading this conversation. Please try again later." ]
render (Conversation state) =
    HH.div [ HP.class_ $ HH.ClassName "card" ] $
    [ HH.h2 [ HP.class_ $ HH.ClassName "card-title" ]
        [ HH.span [ HP.class_ $ HH.ClassName "card-title-text" ]
            [ HH.text $ "Conversation with "
            , HH.a
                [ HP.href $ playerPath state.nickname
                , HE.onClick $ Just <<< Navigate (playerPath state.nickname)]
                [ HH.text $ state.nickname ]
            ]
        , HH.a
            [ HP.class_ $ HH.ClassName "conversation-back"
            , HP.href "/conversations"
            , HE.onClick $ Just <<< Navigate "/conversations"
            ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-arrow-left button-icon" ] []
            , HH.text "Back to conversations"
            ]
        ]
    , HH.div [ HP.class_ $ HH.ClassName "card-section" ]
        if Array.null state.conversation
        then [ HH.p_ [ HH.text "There are no messages in this conversation." ] ]
        else
            (state.conversation <#> \{ nickname, content, created, createdSeconds } ->
                HH.div [ HP.class_ $ HH.ClassName "conversation-message"]
                [ HH.div [ HP.class_ $ HH.ClassName "conversation-message-header" ]
                    [ HH.a
                        [ HP.class_ $ HH.ClassName "conversation-message-nickname"
                        , HP.href $ playerPath nickname
                        , HE.onClick $ Just <<< Navigate (playerPath nickname)]
                        [ HH.text $ nickname ]
                    , divider
                    , HH.span [ HP.class_ $ HH.ClassName "conversation-entry-time" ] [ HH.text $ lastUpdated createdSeconds ]
                    ]
                , HH.div [ HP.class_ $ HH.ClassName "conversation-message-content" ]
                    (content <#> \paragraph -> HH.p [ HP.class_ $ HH.ClassName "conversation-message-content-paragraph" ] [ HH.text paragraph ])
                ])
    , HH.div [ HP.class_ $ HH.ClassName "card-section" ]
        [ inputGroup $
            [ textInput "Write a message..." state.message UpdateMessage
            ]
            <> inputError state.messageError
                "The message cannot be empty and cannot be more than 2000 characters long."
        , HH.button
            [ HP.class_ $ HH.ClassName "primary-button"
            , HP.disabled $ state.message == ""
            , HE.onClick $ Just <<< SendMessage
            ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-envelope button-icon" ] []
            , HH.text "Send message"
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass state.otherError ]
            [ HH.text "There has been an error sending the message. Please try again later." ]
        ]
    ]

loadConversation nickname = Async.unify do
    response <- Fetch.fetch ("/api/conversations/" <> nickname) (Fetch.credentials := Fetch.Include)
        # lmap (const Error)
    conversation :: View.OkContent <- case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # lmap (const Error)
        _ -> Async.left Error
    pure $ Conversation
        { nickname
        , conversation
        , message: ""
        , messageError: false
        , otherError: false
        }

sendMessage state @ { nickname, message } = Async.unify do
    response <- Fetch.fetch
        ("/api/conversations/" <> nickname)
        (  Fetch.method := POST
        <> Fetch.credentials := Fetch.Include
        <> Fetch.body := Json.writeJSON { content: message })
        # lmap (const $ Just $ state { otherError = true })
    case FetchRes.status response of
        204 -> pure Nothing
        400 -> FetchRes.text response >>= JsonAsync.readJSON
            # bimap
                (const $ Just $ state { otherError = true })
                (\(error :: Conversation.BadRequestContent) -> match
                    { invalidMessage: const $ Just $ state { messageError = true } }
                    error)
        _ -> Async.left $ Just $ state { otherError = true }

handleAction :: forall slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Init = do
    state <- H.get
    case state of
        Empty nickname -> do
            newState <- H.lift $ loadConversation nickname
            H.put newState
        Conversation { nickname } -> do
            newState <- H.lift $ loadConversation nickname
            H.put newState
        _ -> pure unit
handleAction (UpdateMessage message) =
    H.modify_ case _ of
        Conversation conversationState -> Conversation conversationState { message = message}
        state -> state
handleAction (SendMessage mouseEvent) = do
    H.liftEffect $ Event.preventDefault $ MouseEvent.toEvent mouseEvent
    state <- H.get
    case state of
        Conversation conversationState -> do
            let resetState = conversationState
                    { messageError = false
                    , otherError = false
                    }
            newState <- H.lift $ sendMessage resetState
            case newState of
                Nothing -> do
                    handleAction Init
                Just newState' -> H.put $ Conversation newState'
        otherState -> pure unit
handleAction (Navigate path mouseEvent) = do
    H.liftEffect $ Event.preventDefault $ MouseEvent.toEvent mouseEvent
    navigate_ path

component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

conversation nickname = HH.slot (SProxy :: SProxy "conversation") unit component nickname absurd
