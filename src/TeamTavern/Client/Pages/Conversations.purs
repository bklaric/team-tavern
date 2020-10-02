module TeamTavern.Client.Pages.Conversations (Slot, conversations) where

import Prelude

import Async (Async(..))
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const(..))
import Data.Either (Either(..))
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Server.Conversation.ViewAll.SendResponse as ViewAll
import Web.Event.Event as Event
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent

type Nickname = String

type Path = String

conversationPath :: Nickname -> Path
conversationPath nickname = "/conversations/" <> nickname

data Action = Init | Navigate Path MouseEvent

data State
    = Empty
    | Conversations ViewAll.OkContent
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
    Just { unit, count } -> show count <> " " <> unit <> (if count == 1 then "" else "s") <> " ago"
    Nothing -> "less than a minute ago"

render Empty = HH.div_ []
render (Conversations conversations') =
    HH.div [ HP.class_ $ HH.ClassName "card" ] $
    [ HH.h2 [ HP.class_ $ HH.ClassName "card-title" ] [ HH.text "Conversations" ] ]
    <>
    if Array.null conversations'
    then Array.singleton $
        HH.div [ HP.class_ $ HH.ClassName "card-section" ]
        [ HH.p_ [ HH.text "Your don't have any conversations." ] ]
    else
        conversations' <#> \{ nickname, unreadMessagesCount, lastMessageCreated, lastMessageCreatedSeconds } ->
            HH.div [ HP.class_ $ HH.ClassName "card-section" ]
            [ HH.div [ HP.class_ $ HH.ClassName "conversation-entry"]
                [ HH.a
                    [ HP.class_ $ HH.ClassName "conversation-entry-nickname"
                    , HP.href $ conversationPath nickname
                    , HE.onClick $ Just <<< Navigate (conversationPath nickname)]
                    [ HH.text $ nickname <>
                        if unreadMessagesCount > 0
                        then " (" <> show unreadMessagesCount <> ")"
                        else ""
                    ]
                , divider
                , HH.span [ HP.class_ $ HH.ClassName "conversation-entry-time" ] [ HH.text $ lastUpdated lastMessageCreatedSeconds ]
                ]
            ]
render Error = HH.p_ [ HH.text "There has been an error loading your conversations. Please try again later." ]

loadConversations = do
    response <- Fetch.fetch "/api/conversations" (Fetch.credentials := Fetch.Include)
        # lmap (const unit)
    content :: ViewAll.OkContent <- case FetchRes.status response of
        200 -> FetchRes.text response >>= Json.readJSON # lmap (const unit)
        _ -> Async.left unit
    pure content

handleAction :: forall slots output left.
    Action -> H.HalogenM State Action slots output (Async left) Unit
handleAction Init = do
    state <- H.lift $ Async.alwaysRight (const Error) Conversations loadConversations
    H.put state
handleAction (Navigate path mouseEvent) = do
    H.liftEffect $ Event.preventDefault $ MouseEvent.toEvent mouseEvent
    H.liftEffect $ navigate_ path

component = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

conversations = HH.slot (SProxy :: SProxy "conversations") unit component unit absurd
