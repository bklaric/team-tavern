module TeamTavern.Client.Components.ContactPanel
    ( ContactPanel
    , PanelActions
    , PanelView
    , Revealed(..)
    , UseContactPanel
    , contactPanel
    , contactPanelSheet
    , contacting
    , contactingOwner
    , contactRows
    , href
    , markMessaged
    , takeContactParam
    , useContactPanel
    ) where

import Prelude

import Async (Async, fromEffectCont)
import Async as Async
import Control.Alt ((<|>))
import Data.Array (catMaybes, elem, find, null)
import Data.DateTime.Instant (Instant)
import Data.Either (hush, isRight)
import Data.Foldable (for_)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.String (Pattern(..), split, stripPrefix)
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Timer (setTimeout)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks (type (<>), Hook, HookM, HookType, Pure, UseState)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Hook (class HookNewtype)
import TeamTavern.Client.Components.Card (postName)
import TeamTavern.Client.Components.Composer (ComposerActions, ComposerState, UseComposer, composer, useComposer)
import TeamTavern.Client.Components.Divider (rule)
import TeamTavern.Client.Components.Overlay (Panel, Presentation(..), UseOverlay, panelHeader, sidePanel, useOverlay)
import TeamTavern.Client.Components.Thread (newFrom, olderPostNote, thread)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Script.Ago (ago)
import TeamTavern.Client.Script.Back (authPath)
import TeamTavern.Client.Script.Clipboard (writeTextAsync)
import TeamTavern.Client.Script.Navigate (navigate_)
import TeamTavern.Client.Script.QueryParams (getQueryParam, removeQueryParam)
import TeamTavern.Client.Script.Thread (scrollThreadsToEnd)
import TeamTavern.Client.Script.Unread (announceUnread)
import TeamTavern.Client.Shared.Contacts (contactLabel)
import TeamTavern.Client.Shared.Fetch (fetchPath, fetchPathBody)
import TeamTavern.Client.Shared.Me (fetchMe)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Conversation.SendMessage (SendMessage)
import TeamTavern.Routes.Conversation.ViewPostConversation (ViewPostConversation)
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Routes.Post.RevealContacts (RevealContacts)
import TeamTavern.Routes.Post.RevealContacts as RevealContacts
import TeamTavern.Routes.Post.ViewPost (ViewPost)
import TeamTavern.Routes.Shared.Card (CardRow)
import TeamTavern.Routes.Shared.Conversation (Conversation)
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Location (pathname)
import Web.HTML.Window (location)

-- The contact panel (brief 5.6): what a post's owner shared to be reached by,
-- and the conversation about the post with its message box. Every contact
-- button opens it, whatever it says.

-- | The contacts and join links, which the panel asks for as it opens, since
-- | the card only knows which there are.
data Revealed = Revealing | Revealed RevealContacts.OkContent | Unrevealed

-- | `copied` is the value whose Copy has just worked. `thread` is the
-- | conversation about the post once the viewer has one, which the panel asks
-- | for as it opens while `loading`.
type ContactPanel =
    { game :: { handle :: String, title :: String }
    , post :: CardRow
    , revealed :: Revealed
    , copied :: Maybe String
    , thread :: Maybe Conversation
    , loading :: Boolean
    }

type PanelActions i = { onClose :: i, onCopy :: String -> i, composer :: ComposerActions i }

-- | The panel with its message box and what they do, which the page draws.
type PanelView i = { panel :: ContactPanel, composer :: ComposerState, actions :: PanelActions i }

type ContactRow = { label :: String, value :: String, link :: Boolean }

panelRef :: H.RefLabel
panelRef = H.RefLabel "contact-panel"

messageRef :: H.RefLabel
messageRef = H.RefLabel "contact-panel-message"

offers :: CardRow -> Boolean
offers post = not null post.contacts || post.has_discord_server || post.has_website

-- An owner who prefers to be added or joined off-site has that come first,
-- unless the viewer has already written (brief 5.6).
contactsLead :: CardRow -> Boolean
contactsLead post =
    offers post && isNothing post.messaged && elem post.contact_preference [ "offsite", "discord", "website" ]

subtitle :: Instant -> ContactPanel -> String
subtitle now { game, post } = game.title <> " " <> post.type <> " · " <> case post.type of
    "player" -> "Active " <> ago now post.updated
    "community" -> "Run by " <> post.owner
    _ -> "Posted by " <> post.owner

contactsHeading :: CardRow -> String
contactsHeading post = case post.contact_preference of
    "offsite" | elem "discord" post.contacts -> "Prefers Discord"
    "offsite" -> "Prefers adding in game"
    "discord" -> "Join on Discord"
    _ -> "Join on their website"

-- | A link as the owner typed it, which may leave out the scheme.
href :: String -> String
href value
    | isJust (stripPrefix (Pattern "https://") value) || isJust (stripPrefix (Pattern "http://") value) = value
    | otherwise = "https://" <> value

rows :: CardRow -> RevealContacts.OkContent -> Array ContactRow
rows post { contacts, discord_server, website } =
    (contacts <#> \{ kind, value } -> { label: contactLabel kind, value, link: kind == "steam" })
    <> catMaybes
        [ discord_server <#> \value ->
            { label: if post.type == "community" then "Discord invite" else "Discord server", value, link: true }
        , website <#> \value -> { label: "Website", value, link: true }
        ]

type Props i = { now :: Instant, view :: PanelView i }

-- A community's invite or website is the panel's one filled button when it
-- comes first.
joinButton :: ∀ w i. ContactPanel -> Array (HH.HTML w i)
joinButton { post, revealed } = case post.type, post.contact_preference, revealed of
    "community", "discord", Revealed { discord_server: Just value } ->
        [ link value [ Icons.discord, HH.text "Open the invite" ] ]
    "community", "website", Revealed { website: Just value } ->
        [ link value [ Icons.externalLink, HH.text "Visit site" ] ]
    _, _, _ -> []
    where
    link value =
        HH.a [ HS.class_ "button button-primary", HP.href $ href value, HP.target "_blank", HP.rel "noopener" ]

contactRow :: ∀ w i. { copied :: Maybe String, onCopy :: String -> i } -> ContactRow -> HH.HTML w i
contactRow { copied, onCopy } { label, value, link } =
    HH.div [ HS.class_ "contact-row" ]
    [ HH.span [ HS.class_ "contact-label" ] [ HH.text label ]
    , if link
        then HH.a [ HS.class_ "contact-value", HP.href $ href value, HP.target "_blank", HP.rel "noopener" ]
            [ HH.text value, Icons.externalLink ]
        else HH.span [ HS.class_ "contact-value" ] [ HH.text value ]
    , HH.button
        [ HS.class_ "button button-outline button-small"
        , HP.type_ HP.ButtonButton
        , HPA.label $ "Copy " <> label
        , HE.onClick $ const $ onCopy value
        ]
        if copied == Just value then [ Icons.check, HH.text "Copied" ] else [ Icons.copy, HH.text "Copy" ]
    ]

-- | The rows of what a post's owner shared, each with Copy, as the panel and
-- | the inbox show them.
contactRows :: ∀ w i.
    { post :: CardRow, revealed :: RevealContacts.OkContent, copied :: Maybe String, onCopy :: String -> i }
    -> HH.HTML w i
contactRows { post, revealed, copied, onCopy } =
    HH.div [ HS.class_ "contact-rows" ] $ contactRow { copied, onCopy } <$> rows post revealed

contactsSection :: ∀ w i. Props i -> Boolean -> HH.HTML w i
contactsSection { view: { panel, actions } } first =
    HH.section [ HS.class_ "panel-section", HPA.label "Contacts" ] $
    (if first then [ HH.h3_ [ HH.text $ contactsHeading panel.post ] ] <> joinButton panel else [])
    <> case panel.revealed of
        Revealing -> [ HH.div [ HPA.busy "true" ] [] ]
        Unrevealed -> [ HH.p [ HS.class_ "muted" ] [ HH.text "The contacts couldn't be shown. Close the panel and try again." ] ]
        Revealed revealed ->
            [ contactRows { post: panel.post, revealed, copied: panel.copied, onCopy: actions.onCopy } ]

-- The message box is the panel's one filled button when it comes first.
messageSection :: ∀ w i. Props i -> Boolean -> HH.HTML w i
messageSection { now, view: { panel: { post, thread: thread', loading }, composer: composerState, actions } } first =
    HH.section [ HS.class_ "panel-section", HPA.label "Conversation" ] $ catMaybes
    [ if first
        then Just $ HH.h3_ [ HH.text $ (if isJust post.messaged then "Your conversation with " else "Message ") <> post.owner ]
        else Nothing
    , olderPostNote post
    , Just case thread' of
        Just conversation ->
            HH.div [ HS.class_ "thread-well" ]
            [ thread
                { now
                , other: conversation.other
                , messages: conversation.messages
                , newFrom: newFrom conversation.readTo conversation.messages
                }
            ]
        Nothing | loading -> HH.div [ HS.class_ "thread-well", HPA.busy "true" ] []
        Nothing -> HH.p [ HS.class_ "muted" ]
            [ HH.text $ "Your message starts a conversation about "
                <> (if post.type == "player" then post.owner <> "'s post" else postName post)
                <> ". Replies show up here and in your inbox."
            ]
    ]
    <> composer { ref: messageRef, primary: first, state: composerState, actions: actions.composer }

panelBody :: ∀ w i. Props i -> Array (HH.HTML w i)
panelBody props@{ view: { panel: { post } } }
    | not offers post = [ messageSection props true ]
    | contactsLead post = [ contactsSection props true, rule "or message on TeamTavern", messageSection props false ]
    | otherwise =
        [ messageSection props true
        , rule if post.type == "community" then "or join directly" else "or add " <> post.owner <> " off-site"
        , contactsSection props false
        ]

panelOf :: ∀ w i. H.RefLabel -> Props i -> Panel w i
panelOf ref { now, view: { panel, actions } } =
    { ref, title: postName panel.post, subtitle: subtitle now panel, tools: [], onClose: actions.onClose }

-- | The panel as a side panel on a desktop and the whole screen on a phone.
contactPanel :: ∀ w i. Instant -> PanelView i -> HH.HTML w i
contactPanel now view = let props = { now, view } in sidePanel (panelOf panelRef props) (panelBody props)

-- | The panel standing still on a page, as the components page shows it.
contactPanelSheet :: ∀ w i. H.RefLabel -> Instant -> PanelView i -> HH.HTML w i
contactPanelSheet ref now view = let props = { now, view } in
    HH.div [ HS.class_ "overlay sheet-panel" ]
    [ panelHeader (panelOf ref props), HH.div [ HS.class_ "overlay-body" ] (panelBody props) ]

-- | The post whose panel the address asks to open, as the account pages return
-- | to it, taken out of the address.
takeContactParam :: ∀ m. MonadEffect m => HookM m (Maybe Int)
takeContactParam = do
    id <- getQueryParam "contact" <#> (_ >>= fromString)
    when (isJust id) $ removeQueryParam "contact"
    pure id

-- | The post an account page's `back` returns to contact, if it does: a page
-- | of the game's with the post in `?contact=`.
contacting :: String -> Maybe { handle :: String, id :: Int }
contacting back = case split (Pattern "?") back of
    [ path, query ] -> do
        handle <- case split (Pattern "/") path of
            [ "", "games", handle ] -> Just handle
            [ "", "games", handle, "posts", _ ] -> Just handle
            _ -> Nothing
        id <- split (Pattern "&") query # find (isJust <<< stripPrefix (Pattern "contact="))
            >>= stripPrefix (Pattern "contact=") >>= fromString
        pure { handle, id }
    _ -> Nothing

-- | Whose post it is, as the account pages name them.
contactingOwner :: ∀ left. { handle :: String, id :: Int } -> Async left (Maybe String)
contactingOwner path =
    Async.attempt (fetchPath (Proxy :: _ ViewPost) path) <#> \result ->
        hush result >>= onMatch { ok: \{ post } -> Just post.owner } (const Nothing)

foreign import data UseContactPanel :: HookType

instance HookNewtype UseContactPanel (UseState (Maybe ContactPanel) <> UseOverlay <> UseComposer <> Pure)

-- | The page's contact panel, given what to do once the viewer has written
-- | about a post, with the time of their first message, so the page's card
-- | can say so. Signed out, opening it leads to sign up, which returns to the
-- | page with the post in `?contact=` for `openPanelById`.
useContactPanel :: ∀ left.
    (Int -> String -> HookM (Async left) Unit)
    -> Hook (Async left) UseContactPanel
        { panel :: Maybe (PanelView (HookM (Async left) Unit))
        , openPanel :: { signedIn :: Boolean, game :: ViewGame.OkContent } -> CardRow -> HookM (Async left) Unit
        , openPanelById :: ViewGame.OkContent -> Int -> HookM (Async left) Unit
        , closePanel :: HookM (Async left) Unit
        }
useContactPanel onMessaged = Hooks.wrap Hooks.do
    panel /\ panelId <- Hooks.useState Nothing

    let closePanel = Hooks.put panelId Nothing
        update id f = Hooks.modify_ panelId $ map \panel' -> if panel'.post.id == id then f panel' else panel'

    useOverlay panelRef Side (isJust panel) closePanel

    let send draft = Hooks.get panelId >>= case _ of
            Nothing -> pure false
            Just { game, post } -> do
                result <- H.lift $ Async.attempt $
                    fetchPathBody (Proxy :: _ SendMessage) { handle: game.handle, id: post.id } { content: draft }
                case hush result >>= onMatch { ok: Just } (const Nothing) of
                    Just conversation -> do
                        let messaged = post.messaged <|> (find _.mine conversation.messages <#> _.created)
                        update post.id _ { thread = Just conversation, post = post { messaged = messaged } }
                        for_ messaged $ onMessaged post.id
                        liftEffect do
                            announceUnread
                            scrollThreadsToEnd
                        pure true
                    Nothing -> pure false

    message <- useComposer messageRef send

    let openPanel { signedIn, game } post
            | not signedIn = do
                path <- liftEffect $ window >>= location >>= pathname
                navigate_ $ authPath "/signup" $ path <> "?contact=" <> show post.id
            | otherwise = do
                message.clear
                Hooks.put panelId $ Just
                    { game: { handle: game.handle, title: game.title }
                    , post
                    , revealed: if offers post then Revealing else Revealed { contacts: [], discord_server: Nothing, website: Nothing }
                    , copied: Nothing
                    , thread: Nothing
                    , loading: isJust post.messaged
                    }
                when (offers post) $ void $ Hooks.fork do
                    result <- H.lift $ Async.attempt $ fetchPath (Proxy :: _ RevealContacts) { handle: game.handle, id: post.id }
                    let revealed = hush result >>= onMatch { ok: Just } (const Nothing) # maybe Unrevealed Revealed
                    update post.id _ { revealed = revealed }
                -- Reading the conversation here marks it read, as the inbox does.
                when (isJust post.messaged) $ void $ Hooks.fork do
                    result <- H.lift $ Async.attempt $
                        fetchPath (Proxy :: _ ViewPostConversation) { handle: game.handle, id: post.id }
                    let thread' = hush result >>= onMatch { ok: _.conversation } (const Nothing)
                    update post.id \panel' -> panel' { thread = panel'.thread <|> thread', loading = false }
                    liftEffect do
                        announceUnread
                        scrollThreadsToEnd

        -- Only for a player signed in, and a post they can't contact opens
        -- nothing.
        openPanelById game id = void $ Hooks.fork do
            me <- H.lift fetchMe
            result <- H.lift $ Async.attempt $ fetchPath (Proxy :: _ ViewPost) { handle: game.handle, id }
            case me, hush result >>= onMatch { ok: Just } (const Nothing) of
                Just _, Just { post, blocked: Nothing } | not post.own -> openPanel { signedIn: true, game } post
                _, _ -> pure unit

        copy value = void $ Hooks.fork do
            written <- H.lift $ Async.attempt $ writeTextAsync value
            when (isRight written) do
                id <- Hooks.get panelId <#> map _.post.id
                for_ id \id' -> do
                    update id' _ { copied = Just value }
                    H.lift $ fromEffectCont \done -> void $ setTimeout 2000 $ done unit
                    update id' \panel' -> if panel'.copied == Just value then panel' { copied = Nothing } else panel'

        view = panel <#> \panel' ->
            { panel: panel'
            , composer: message.state
            , actions: { onClose: closePanel, onCopy: copy, composer: message.actions }
            }

    Hooks.pure { panel: view, openPanel, openPanelById, closePanel }

-- | The card of the post written about, once the viewer has, with the time of
-- | their first message.
markMessaged :: Int -> String -> CardRow -> CardRow
markMessaged id time post
    | post.id == id = post { messaged = post.messaged <|> Just time }
    | otherwise = post
