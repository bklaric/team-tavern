module TeamTavern.Client.Components.Conversation.Conversation
    (Slot, conversation) where

import Prelude

import Async (Async(..))
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON as Json
import Simple.JSON.Async as JsonAsync
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Textarea (Message(..), Query(..), Text, textarea) as Textarea
import TeamTavern.Client.Script.Navigate (navigate_)
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
    | MessageChanged Textarea.Text
    | SendMessage Nickname Textarea.Text MouseEvent
    | Navigate Path MouseEvent

data State
    = Empty Nickname
    | Conversation Nickname Textarea.Text View.OkContent
    | Error

type Slot = H.Slot (Const Void) Void Unit

yearSeconds :: Number
yearSeconds = 60.0 * 60.0 * 24.0 * 365.0

monthSeconds :: Number
monthSeconds = 60.0 * 60.0 * 24.0 * 30.0

daySeconds :: Number
daySeconds = 60.0 * 60.0 * 24.0

hourSeconds :: Number
hourSeconds = 60.0 * 60.0

minuteSeconds :: Number
minuteSeconds = 60.0

lastUpdated :: Number -> String
lastUpdated updatedSeconds = let
    yearsAgo = floor(updatedSeconds / yearSeconds)
    monthsAgo = floor(updatedSeconds / monthSeconds)
    daysAgo = floor(updatedSeconds / daySeconds)
    hoursAgo = floor(updatedSeconds / hourSeconds)
    minutesAgo = floor(updatedSeconds / minuteSeconds)
    interval =
        if yearsAgo > 0 then Just { unit: "year", count: yearsAgo } else
        if monthsAgo > 0 then Just { unit: "month", count: monthsAgo } else
        if daysAgo > 0 then Just { unit: "day", count: daysAgo } else
        if hoursAgo > 0 then Just { unit: "hour", count: hoursAgo } else
        if minutesAgo > 0 then Just { unit: "minute", count: minutesAgo } else
        Nothing
    in
    case interval of
    Just { unit, count } -> show count <> " " <> unit <> if count == 1 then "" else "s" <> " ago"
    Nothing -> "less than a minute ago"

render (Empty _) = HH.div_ []
render (Conversation nickname' messageText conversation') =
    HH.div [ HP.class_ $ HH.ClassName "card" ] $
    [ HH.h2 [ HP.class_ $ HH.ClassName "card-title" ]
        [ HH.text $ "Conversation with "
        , HH.a
            [ HP.href $ playerPath nickname'
            , HE.onClick $ Just <<< Navigate (playerPath nickname')]
            [ HH.text $ nickname' ]
        ]
    , HH.div [ HP.class_ $ HH.ClassName "card-section" ]
        if Array.null conversation'
        then [ HH.p_ [ HH.text "There are no messages in this conversation." ] ]
        else
            (conversation' <#> \{ nickname, content, created, createdSeconds } ->
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
        [ HH.div [ HP.class_ $ HH.ClassName "conversation-input" ]
            [ Textarea.textarea
                (SProxy :: SProxy "newMessage")
                { text: "", placeholder: "Write a message..." }
                \(Textarea.TextChanged message) -> Just $ MessageChanged message
            ]
        , HH.button
            [ HP.class_ $ HH.ClassName "primary-button"
            , HP.disabled $ messageText == ""
            , HE.onClick $ Just <<< SendMessage nickname' messageText
            ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-envelope button-icon" ] []
            , HH.text "Send message"
            ]
        ]
    ]
render Error = HH.p_ [ HH.text "There has been an error loading this conversation. Please try again later." ]

loadConversation nickname = do
    response <- Fetch.fetch ("/api/conversations/" <> nickname) (Fetch.credentials := Fetch.Include)
        # lmap (const unit)
    content :: View.OkContent <- case FetchRes.status response of
        200 -> FetchRes.text response >>= JsonAsync.readJSON # lmap (const unit)
        _ -> Async.left unit
    pure content

sendMessage nickname message = do
    response <- Fetch.fetch
        ("/api/conversations/" <> nickname)
        (  Fetch.method := POST
        <> Fetch.credentials := Fetch.Include
        <> Fetch.body := Json.writeJSON { content: message })
        # lmap (const unit)
    case FetchRes.status response of
        204 -> pure unit
        _ -> Async.left unit

handleAction Init = do
    state <- H.get
    case state of
        Empty nickname -> do
            state' <- H.lift $ Async.alwaysRight (const Error) (Conversation nickname "") (loadConversation nickname)
            H.put state'
        Conversation nickname _ _ -> do
            state' <- H.lift $ Async.alwaysRight (const Error) (Conversation nickname "") (loadConversation nickname)
            H.put state'
        _ -> pure unit
handleAction (MessageChanged message) =
    H.modify_ case _ of
        Conversation nickname _ messages -> Conversation nickname message messages
        state -> state
handleAction (SendMessage nickname message mouseEvent) = do
    H.liftEffect $ Event.preventDefault $ MouseEvent.toEvent mouseEvent
    result <- H.lift $ Async.attempt $ sendMessage nickname message
    case result of
        Left _ -> pure unit
        Right _ -> do
            void $ H.query (SProxy :: SProxy "newMessage") unit (Textarea.Clear unit)
            handleAction Init
handleAction (Navigate path mouseEvent) = do
    H.liftEffect $ Event.preventDefault $ MouseEvent.toEvent mouseEvent
    H.liftEffect $ navigate_ path

component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

conversation nickname = HH.slot (SProxy :: SProxy "conversation") unit component nickname absurd
