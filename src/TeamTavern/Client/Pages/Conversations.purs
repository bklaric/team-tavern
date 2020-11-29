module TeamTavern.Client.Pages.Conversations (Slot, conversations) where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Options ((:=))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Card (card, cardHeader, cardHeading)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Script.LastUpdated (lastUpdated)
import TeamTavern.Client.Script.Meta (setMeta)
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

render :: forall t81. State -> HH.HTML t81 Action
render Empty = HH.div_ []
render (Conversations conversations') =
    card $
    [ cardHeader [ cardHeading "Conversations" ] ]
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

loadConversations :: Async Unit ViewAll.OkContent
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
    setMeta "Conversations | TeamTavern" "View all your conversations."
handleAction (Navigate path mouseEvent) = do
    H.liftEffect $ Event.preventDefault $ MouseEvent.toEvent mouseEvent
    navigate_ path

component :: forall t174 t199 t202 t205. H.Component HH.HTML t202 t199 t174 (Async t205)
component = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

conversations :: forall t209 t216 t217 t219.
  HH.HTML
    (H.ComponentSlot HH.HTML
       ( conversations :: H.Slot t217 Void Unit
       | t209
       )
       (Async t219)
       t216
    )
    t216
conversations = HH.slot (SProxy :: SProxy "conversations") unit component unit absurd
