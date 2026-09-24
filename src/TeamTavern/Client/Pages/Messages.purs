module TeamTavern.Client.Pages.Messages (Input, messages) where

import Prelude

import Async (Async, fromEffectCont)
import Async as Async
import Data.Array (length, null)
import Data.Either (hush, isRight)
import Data.Foldable (for_, traverse_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Timer (setTimeout)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks (HookM)
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.BlockReport (blockReportBody, moreMenu, useBlockReport)
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button, buttonLink)
import TeamTavern.Client.Components.Card (Viewer, postFacts, postName)
import TeamTavern.Client.Components.Composer (composer, useComposer)
import TeamTavern.Client.Components.ContactPanel (Revealed(..), contactRows)
import TeamTavern.Client.Components.EmptyState (emptyState)
import TeamTavern.Client.Components.InboxRow (inboxCover, inboxPostName, inboxRow)
import TeamTavern.Client.Components.Thread (newFrom, olderPostNote, thread)
import TeamTavern.Client.Components.Toast (toasts, useToast)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Script.Ago (ago)
import TeamTavern.Client.Script.Back (authPath)
import TeamTavern.Client.Script.Clipboard (writeTextAsync)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_, navigate_)
import TeamTavern.Client.Script.Thread (isWide, scrollThreadsToEnd)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Script.Unread (announceUnread)
import TeamTavern.Client.Shared.Block (reportConversation)
import TeamTavern.Client.Shared.Fetch (fetchPath, fetchPathBody, fetchSimple)
import TeamTavern.Client.Shared.Renew (renew, renewFailed) as Renew
import TeamTavern.Client.Shared.Slot (Slot__I)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Conversation.SendReply (SendReply)
import TeamTavern.Routes.Conversation.ViewConversation (ViewConversation)
import TeamTavern.Routes.Conversation.ViewInbox (InboxRow, ViewInbox)
import TeamTavern.Routes.Conversation.ViewInbox as ViewInbox
import TeamTavern.Routes.Game.ViewGame (ViewGame)
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Routes.Post.RevealContacts (RevealContacts)
import TeamTavern.Routes.Shared.Card (CardRow)
import TeamTavern.Routes.Shared.Conversation (Conversation)
import Type.Proxy (Proxy(..))
import Web.HTML.HTMLElement (focus)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

-- The inbox (brief 10): conversations grouped by the post they are about, the
-- player's own posts first, and the open one beside the list on a desktop or
-- on its own screen below that. One component serves /messages and
-- /messages/:id, so choosing a conversation keeps the list drawn.

-- | `visit` counts the router's navigations, each of which asks afresh.
type Input = { conversation :: Maybe Int, visit :: Int }

data Inbox = Loading | SignedOut | Failed | Loaded ViewInbox.OkContent

-- | The conversation open with the game its facts are read by. `new` is where
-- | its New line went when it was opened, kept while it stays open.
type Opened =
    { conversation :: Conversation
    , game :: ViewGame.OkContent
    , new :: Maybe Int
    , revealed :: Maybe Revealed
    , copied :: Maybe String
    }

data Open = Closed | Opening Int | Open Opened | Gone

type State =
    { inbox :: Inbox
    , open :: Open
    , games :: Map String ViewGame.OkContent
    , viewer :: Maybe Viewer
    , visit :: Int
    }

messageRef :: H.RefLabel
messageRef = H.RefLabel "conversation-message"

menuRef :: H.RefLabel
menuRef = H.RefLabel "conversation-menu"

conversationPath :: Int -> String
conversationPath id = "/messages/" <> show id

plural :: Int -> String -> String
plural count noun = show count <> " " <> noun <> if count == 1 then "" else "s"

-- The title the open conversation gives the page: the other player about the
-- viewer's own post, the post about someone else's.
titleOf :: Conversation -> String
titleOf conversation = if conversation.post.own then conversation.other else postName conversation.post

offers :: CardRow -> Boolean
offers post = not null post.contacts || post.has_discord_server || post.has_website

component :: ∀ query output left. H.Component query Input output (Async left)
component = Hooks.component \_ { conversation: openId, visit } -> Hooks.do
    state /\ stateId <- Hooks.useState
        ({ inbox: Loading, open: Closed, games: Map.empty, viewer: Nothing, visit } :: State)
    { toast, showToast, dismissToast } <- useToast

    let set = Hooks.modify_ stateId

        -- An answer counts only while its visit is the latest.
        whileCurrent visit' f = set \state' -> if state'.visit == visit' then f state' else state'

        loadInbox visit' = do
            result <- H.lift $ Async.attempt $ fetchSimple (Proxy :: _ ViewInbox)
            let inbox = case hush result of
                    Just response -> response # onMatch
                        { ok: Loaded, notAuthorized: const SignedOut } (const Failed)
                    Nothing -> Failed
            whileCurrent visit' _ { inbox = inbox }

        gameOf handle = do
            games <- Hooks.get stateId <#> _.games
            case Map.lookup handle games of
                Just game -> pure $ Just game
                Nothing -> do
                    result <- H.lift $ Async.attempt $ fetchPath (Proxy :: _ ViewGame) { handle }
                    let game = hush result >>= onMatch { ok: Just } (const Nothing)
                    for_ game \game' -> set \state' -> state' { games = Map.insert handle game' state'.games }
                    pure game

        -- Opening a conversation marks it read, which its row and the header
        -- then show.
        loadConversation visit' id = do
            result <- H.lift $ Async.attempt $ fetchPath (Proxy :: _ ViewConversation) { id }
            case hush result >>= onMatch { ok: Just } (const Nothing) of
                Nothing -> do
                    whileCurrent visit' _ { open = Gone }
                    setMeta "Messages | TeamTavern" ""
                Just conversation -> do
                    game' <- gameOf conversation.game.handle
                    case game' of
                        Nothing -> whileCurrent visit' _ { open = Gone }
                        Just game -> do
                            whileCurrent visit' \state' -> state'
                                { open = Open
                                    { conversation
                                    , game
                                    , new: newFrom conversation.readTo conversation.messages
                                    , revealed: Nothing
                                    , copied: Nothing
                                    }
                                , inbox = case state'.inbox of
                                    Loaded inbox -> Loaded $ markRead id inbox
                                    inbox -> inbox
                                }
                            setMeta (titleOf conversation <> " · Messages | TeamTavern") ""
                            liftEffect do
                                announceUnread
                                scrollThreadsToEnd
                            wide <- liftEffect isWide
                            when wide $ Hooks.getHTMLElementRef messageRef >>= traverse_ (liftEffect <<< focus)

        markRead id inbox = let
            read row = if row.id == id then row { unread = false } else row
            in
            { own: inbox.own <#> \group -> group { conversations = read <$> group.conversations }
            , messaged: read <$> inbox.messaged
            }

        send draft = Hooks.get stateId <#> _.open >>= case _ of
            Open opened -> do
                result <- H.lift $ Async.attempt $
                    fetchPathBody (Proxy :: _ SendReply) { id: opened.conversation.id } { content: draft }
                case hush result >>= onMatch { ok: Just } (const Nothing) of
                    Just conversation -> do
                        set \state' -> case state'.open of
                            Open opened' | opened'.conversation.id == conversation.id ->
                                state' { open = Open opened' { conversation = conversation, new = Nothing } }
                            _ -> state'
                        liftEffect do
                            announceUnread
                            scrollThreadsToEnd
                        visit' <- Hooks.get stateId <#> _.visit
                        void $ Hooks.fork $ loadInbox visit'
                        pure true
                    Nothing -> pure false
            _ -> pure false

    message <- useComposer messageRef send

    -- A block takes the conversation out of both inboxes, so the list shows
    -- without it and with nothing chosen. Undo puts it back in the list.
    { blockReport, back, reset } <- useBlockReport
        { ref: menuRef
        , scope: ".conversation"
        , subject: Hooks.get stateId <#> \state' -> case state'.open of
            Open { conversation } -> Just { who: conversation.other, report: reportConversation conversation.id }
            _ -> Nothing
        , close: navigate_ "/messages"
        , changed: Hooks.get stateId <#> _.visit >>= loadInbox
        , showToast
        }

    -- Every visit asks for the inbox again, and for the conversation it opens.
    -- The inbox and the conversation are asked for at once; the row of the one
    -- opened is marked read here, whichever answers first.
    Hooks.captures { visit } Hooks.useTickEffect do
        now' <- liftEffect now
        timezone <- getClientTimezone
        previous <- Hooks.get stateId <#> _.open
        let same = case previous, openId of
                Open { conversation }, Just id -> conversation.id == id
                _, _ -> false
        unless same do
            message.clear
            reset
        set _
            { visit = visit
            , viewer = Just { now: now', timezone }
            , open = if same then previous else maybe Closed Opening openId
            }
        when (openId == Nothing) $ setMeta "Messages | TeamTavern" ""
        void $ Hooks.fork $ loadInbox visit
        for_ openId \id -> void $ Hooks.fork $ loadConversation visit id
        pure Nothing

    let renewPost :: String -> { id :: Int, type :: String } -> HookM (Async left) Unit
        renewPost handle post = void $ Hooks.fork do
            renewed <- H.lift $ Renew.renew handle post
            case renewed of
                Nothing -> showToast { text: Renew.renewFailed, action: Nothing }
                Just text -> do
                    visit' <- Hooks.get stateId <#> _.visit
                    loadInbox visit'
                    open <- Hooks.get stateId <#> _.open
                    case open of
                        Open { conversation } | conversation.post.id == post.id ->
                            loadConversation visit' conversation.id
                        _ -> pure unit
                    showToast { text, action: Nothing }

        -- The owner's contacts are asked for as the fold first opens, which
        -- counts as a reveal, as opening the contact panel does.
        reveal = Hooks.get stateId <#> _.open >>= case _ of
            Open { conversation, revealed: Nothing } -> do
                let update revealed = set \state' -> case state'.open of
                        Open opened | opened.conversation.id == conversation.id ->
                            state' { open = Open opened { revealed = Just revealed } }
                        _ -> state'
                update Revealing
                void $ Hooks.fork do
                    result <- H.lift $ Async.attempt $ fetchPath (Proxy :: _ RevealContacts)
                        { handle: conversation.game.handle, id: conversation.post.id }
                    update $ hush result >>= onMatch { ok: Just } (const Nothing) # maybe Unrevealed Revealed
            _ -> pure unit

        copy value = void $ Hooks.fork do
            written <- H.lift $ Async.attempt $ writeTextAsync value
            when (isRight written) do
                let copied value' = set \state' -> case state'.open of
                        Open opened -> state' { open = Open opened { copied = value' } }
                        _ -> state'
                copied $ Just value
                H.lift $ fromEffectCont \done -> void $ setTimeout 2000 $ done unit
                set \state' -> case state'.open of
                    Open opened | opened.copied == Just value -> state' { open = Open opened { copied = Nothing } }
                    _ -> state'

        rowHtml :: Viewer -> Boolean -> InboxRow -> Tuple String (HH.HTML _ (HookM (Async left) Unit))
        rowHtml viewer ownPost row =
            Tuple (show row.id) $ inboxRow { now: viewer.now, current: openId == Just row.id, ownPost, row }

        -- A post of the player's heads its conversations, one per player who
        -- wrote about it. An expired one can be renewed from here.
        groupHtml viewer { post, conversations } =
            Tuple ("post-" <> show post.id) $ HH.div [ HS.class_ "inbox-group" ]
            [ HH.div [ HS.class_ "inbox-post" ] $
                [ inboxCover post.handle
                , HH.span [ HS.class_ "inbox-post-text" ]
                    [ HH.span [ HS.class_ "inbox-post-name" ] [ HH.text $ inboxPostName post ]
                    , HH.span [ HS.class_ "inbox-kind" ]
                        [ HH.text $ post.game <> " " <> post.type
                            <> " · " <> plural (length conversations) "conversation"
                            <> if post.expired then " · Expired" else ""
                        ]
                    ]
                ]
                <> if post.expired
                    then [ button Outline Small (renewPost post.handle { id: post.id, type: post.type })
                        [ Icons.refreshCw, HH.text "Renew" ] ]
                    else []
            , HK.div_ $ rowHtml viewer true <$> conversations
            ]

        listHtml viewer inbox =
            HH.nav [ HS.class_ "inbox", HPA.labelledBy "inbox-title" ] $
            [ HH.h1 [ HP.id "inbox-title" ] [ HH.text "Messages" ] ]
            <> (if null inbox.own then []
                else
                    [ HH.h2 [ HS.class_ "inbox-heading" ] [ HH.text "Your posts" ]
                    , HK.div_ $ groupHtml viewer <$> inbox.own
                    ])
            <> (if null inbox.messaged then []
                else
                    [ HH.h2 [ HS.class_ "inbox-heading" ] [ HH.text "Posts you messaged" ]
                    , HK.div_ $ rowHtml viewer false <$> inbox.messaged
                    ])

        -- About the player's own post, the header is the other player: their
        -- post's facts if they have one in the game. About someone else's post,
        -- the header is the post, with its owner's contacts a click away.
        headerHtml viewer { conversation, game, revealed, copied } = let
            post = conversation.post
            kind = game.title <> " " <> post.type
            backLink = HH.a
                [ HS.class_ "icon-button messages-back"
                , HP.href "/messages"
                , HPA.label "All messages"
                , HE.onClick $ navigateWithEvent_ "/messages"
                ]
                [ Icons.arrowLeft ]
            lines
                | post.own =
                    ( conversation.otherPost >>= \theirs ->
                        postFacts game viewer
                            (if theirs.type == "player" then Nothing
                                else Just $ "Their " <> theirs.type <> " " <> postName theirs)
                            theirs
                    )
                    # maybe [] pure
                    # (_ <> [ HH.div [ HS.class_ "conversation-context" ] $
                        [ HH.text $ "About your post " <> postName post <> " · " <> kind
                            <> if post.expired then " · Expired" else ""
                        ]
                        <> if post.expired
                            then [ button Text Small (renewPost game.handle { id: post.id, type: post.type })
                                [ Icons.refreshCw, HH.text "Renew" ] ]
                            else []
                    ])
                | otherwise =
                    [ HH.div [ HS.class_ "conversation-context" ]
                        [ HH.text $ kind <> " · " <> case post.type of
                            "player" -> "Active " <> ago viewer.now post.updated
                            "community" -> "Run by " <> post.owner
                            _ -> "Posted by " <> post.owner
                        ]
                    ]
                    <> maybe [] pure (postFacts game viewer Nothing post)
                    <> if offers post
                        then
                            [ HH.details [ HS.class_ "thread-contacts" ]
                                [ HH.summary [ HE.onClick $ const reveal ]
                                    [ HH.text if post.type == "community" then "Ways to join" else post.owner <> "'s contacts"
                                    , Icons.chevronDown
                                    ]
                                , case revealed of
                                    Just (Revealed revealed') ->
                                        contactRows { post, revealed: revealed', copied, onCopy: copy }
                                    Just Unrevealed -> HH.p [ HS.class_ "muted" ]
                                        [ HH.text "The contacts couldn't be shown. Try again later." ]
                                    Just Revealing -> HH.div [ HPA.busy "true" ] []
                                    Nothing -> HH.div_ []
                                ]
                            ]
                        else []
            in
            HH.div [ HS.class_ "conversation-header" ]
            [ backLink
            , HH.div [ HS.class_ "conversation-title" ] $
                [ HH.h2 [ HP.id "conversation-title" ] [ HH.text $ titleOf conversation ] ] <> lines
            , moreMenu menuRef { who: conversation.other, reportLabel: "Report " <> conversation.other } blockReport
            ]

        -- Blocking and reporting stand in place of the thread and the message
        -- box, and Escape leaves them for the thread.
        conversationHtml viewer = case state.open of
            Open opened@{ conversation, new } ->
                HH.section
                [ HS.class_ "conversation"
                , HPA.labelledBy "conversation-title"
                , HE.onKeyDown \event -> when (KeyboardEvent.key event == "Escape") $ void back
                ] $
                [ headerHtml viewer opened ]
                <> case blockReportBody { who: conversation.other, reportTitle: conversation.other } blockReport of
                    Just body -> [ HH.div [ HS.class_ "conversation-body" ] [ body ] ]
                    Nothing ->
                        [ HH.div [ HS.class_ "conversation-body" ] $
                            (if conversation.post.own then [] else maybe [] pure (olderPostNote conversation.post))
                            <>
                            [ thread
                                { now: viewer.now
                                , other: conversation.other
                                , messages: conversation.messages
                                , newFrom: new
                                }
                            ]
                        , HH.div [ HS.class_ "conversation-composer" ] $
                            composer { ref: messageRef, primary: true, state: message.state, actions: message.actions }
                        ]
            Opening _ -> HH.section [ HS.class_ "conversation", HPA.busy "true" ] []
            _ -> HH.section [ HS.class_ "conversation" ]
                [ HH.div [ HS.class_ "conversation-empty" ] [ HH.text "Choose a conversation." ] ]

        -- A block can empty the inbox, and its toast still shows.
        alone content = HH.div [ HS.class_ "messages-alone" ] [ content, toasts toast dismissToast ]

    Hooks.pure case state.inbox, state.viewer of
        Loaded inbox, Just viewer
            | null inbox.own && null inbox.messaged ->
                alone $ emptyState
                    { heading: "No messages yet"
                    , text: "Message someone from a game's feed, or publish a post: players who fit it can message you, and the conversation shows up here."
                    , action: buttonLink Primary Regular "/post" [ HH.text "Publish a post" ]
                    }
            | otherwise ->
                HH.div
                    ( [ HS.class_ "messages-page" ]
                      <> if isJust openId then [ HP.attr (HH.AttrName "data-open") "" ] else []
                    )
                [ listHtml viewer inbox, conversationHtml viewer, toasts toast dismissToast ]
        SignedOut, _ ->
            alone $ emptyState
                { heading: "Sign in to see your messages"
                , text: "Conversations about your posts, and about the posts you message, are kept here."
                , action: buttonLink Primary Regular (authPath "/signin" $ maybe "/messages" conversationPath openId)
                    [ HH.text "Sign in" ]
                }
        Failed, _ -> alone $ HH.p_ [ HH.text "There has been an error loading your messages." ]
        _, _ -> HH.div [ HS.class_ "messages-alone", HPA.busy "true" ] []

-- | One slot for the list and for every conversation, so choosing one keeps
-- | the list as it is while the conversation arrives.
messages :: ∀ action slots left.
    Input -> H.ComponentHTML action (messages :: Slot__I Unit | slots) (Async left)
messages input = HH.slot_ (Proxy :: _ "messages") unit component input
