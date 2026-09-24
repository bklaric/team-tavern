module TeamTavern.Client.Pages.PostPage (Input, postPage) where

import Prelude

import Async (Async)
import Async as Async
import Control.Alt ((<|>))
import Control.Parallel (parallel, sequential)
import Data.Array (catMaybes, find, mapMaybe)
import Data.Either (Either(..), hush)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (joinWith, trim)
import Data.String.CodeUnits as CodeUnits
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks as Hooks
import TeamTavern.Client.Components.Ads as Ads
import TeamTavern.Client.Components.Card (Place(..), Viewer, card, postName, typeIcon)
import TeamTavern.Client.Components.ContactPanel (contactPanel, markMessaged, takeContactParam, useContactPanel)
import TeamTavern.Client.Components.OwnPostStatus (ownPostStatus)
import TeamTavern.Client.Components.Toast (toasts, useToast)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Pages.Feed.Description (current, loadStored, storeDescription, storedFrom)
import TeamTavern.Client.Pages.Feed.Fields (barFields, summary)
import TeamTavern.Client.Pages.Placeholder (placeholder)
import TeamTavern.Client.Script.Meta (setMeta, setMetaRobots)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_, navigate_)
import TeamTavern.Client.Script.RenderReady (appendRenderReadyNotFound, appendRenderReadyUnavailable)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Shared.Fetch (fetchPath)
import TeamTavern.Client.Shared.Me (fetchMe)
import TeamTavern.Client.Shared.Renew (renew, renewFailed)
import TeamTavern.Client.Shared.Slot (Slot__I)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Feed.ViewOwnDescriptions (OwnDescription, ViewOwnDescriptions)
import TeamTavern.Routes.Game.ViewGame (ViewGame)
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Routes.Post.ViewPost (ViewPost)
import TeamTavern.Routes.Post.ViewPost as ViewPost
import Type.Proxy (Proxy(..))
import Web.Event.Event (preventDefault)
import Web.HTML (window)
import Web.HTML.History (back)
import Web.HTML.Window (history)
import Web.UIEvent.MouseEvent (MouseEvent, altKey, button, ctrlKey, metaKey, shiftKey, toEvent)

-- | `feedBehind` is whether the browser's Back returns to the game's feed.
type Input = { handle :: String, id :: Int, feedBehind :: Boolean }

-- | `described` is the description the game's feed opens with for the viewer,
-- | as its type and summary, where it gives anything.
type Shown =
    { game :: ViewGame.OkContent
    , page :: ViewPost.OkContent
    , signedIn :: Boolean
    , own :: Array OwnDescription
    , described :: Maybe { type :: String, text :: String }
    }

data Page = Loading | Shown Shown | Gone ViewGame.OkContent | Missing | Failed

type State = { page :: Page, viewer :: Maybe Viewer }

typeName :: ViewGame.OkContent -> String -> String
typeName game type_ = game.title <> " " <> type_

-- A search result shows the post's own words.
metaDescription :: ViewGame.OkContent -> ViewPost.OkContent -> String
metaDescription game { post } = let
    text = post.summary # joinWith " " # trim
    in
    if text == "" then postName post <> ", a " <> typeName game post.type <> " post on TeamTavern."
    else if CodeUnits.length text > 155 then trim (CodeUnits.take 154 text) <> "…"
    else text

-- A click with a modifier opens the link in another tab or window, which the
-- browser does itself from the link's href.
goBack :: ∀ m. MonadEffect m => MouseEvent -> m Unit
goBack event = liftEffect do
    let modified = ctrlKey event || metaKey event || shiftKey event || altKey event
    unless (modified || button event /= 0) do
        preventDefault $ toEvent event
        window >>= history >>= back

component :: ∀ query output left. H.Component query Input output (Async left)
component = Hooks.component \_ { handle, id, feedBehind } -> Hooks.do
    state /\ stateId <- Hooks.useState ({ page: Loading, viewer: Nothing } :: State)
    { toast, showToast, dismissToast } <- useToast

    -- The page reads the post again, which a block leaves without its contact
    -- button, under the line saying why, and Undo gives back.
    let readPost = do
            result <- H.lift $ Async.attempt $ fetchPath (Proxy :: _ ViewPost) { handle, id }
            for_ (hush result >>= onMatch { ok: Just } (const Nothing)) \page ->
                Hooks.modify_ stateId \state' -> case state'.page of
                    Shown shown -> state' { page = Shown shown { page = page } }
                    _ -> state'

    { panel, openPanel, openPanelById } <- useContactPanel
        { onMessaged: \id' time -> Hooks.modify_ stateId \state' -> case state'.page of
            Shown shown -> state' { page = Shown shown { page = shown.page { post = markMessaged id' time shown.page.post } } }
            _ -> state'
        , onBlockChange: void $ Hooks.fork readPost
        , showToast
        }

    let feedPath = "/games/" <> handle

    Hooks.useLifecycleEffect do
        now' <- liftEffect now
        timezone <- getClientTimezone
        Hooks.modify_ stateId _ { viewer = Just { now: now', timezone } }
        void $ Hooks.fork do
            Tuple game post <- H.lift $ sequential $ Tuple
                <$> parallel (Async.attempt $ fetchPath (Proxy :: _ ViewGame) { handle })
                <*> parallel (Async.attempt $ fetchPath (Proxy :: _ ViewPost) { handle, id })
            let failed = do
                    appendRenderReadyUnavailable
                    Hooks.modify_ stateId _ { page = Failed }
                notFound = do
                    appendRenderReadyNotFound
                    setMeta "Page not found | TeamTavern" ""
                    Hooks.modify_ stateId _ { page = Missing }
            case game, post of
                Right game', Right post' -> game' # onMatch
                    { ok: \game'' -> post' # onMatch
                        { ok: \page -> do
                            -- The viewer's own posts describe the feed while they
                            -- haven't stored a description, and the owner's post is
                            -- what See what fits describes it with.
                            me <- H.lift fetchMe
                            own <- if isJust me
                                then H.lift (Async.attempt $ fetchPath (Proxy :: _ ViewOwnDescriptions) { handle })
                                    <#> (hush >=> onMatch { ok: Just } (const Nothing)) <#> fromMaybe []
                                else pure []
                            stored <- liftEffect $ loadStored handle
                            let description = stored <|> storedFrom own <#> current
                                described = description >>= \description' ->
                                    case barFields game'' { countries: [], regions: [], languages: [] } description'.type
                                        # mapMaybe (flip summary description') of
                                    [] -> Nothing
                                    parts -> Just { type: description'.type, text: joinWith " · " parts }
                            setMeta
                                (postName page.post <> " · " <> typeName game'' page.post.type <> " | TeamTavern")
                                (metaDescription game'' page)
                            -- An expired post keeps its page, but out of search
                            -- engines until it is renewed (brief 11.1).
                            when page.post.expired $ setMetaRobots "noindex"
                            Hooks.modify_ stateId _
                                { page = Shown { game: game'', page, signedIn: isJust me, own, described } }
                            -- Back from signing up to contact the post, its panel opens.
                            takeContactParam >>= traverse_ (openPanelById game'')
                        , notFound: const do
                            appendRenderReadyNotFound
                            setMeta ("This post is gone · " <> game''.title <> " | TeamTavern")
                                ("The " <> game''.title <> " feed has everyone who is looking on TeamTavern.")
                            Hooks.modify_ stateId _ { page = Gone game'' }
                        }
                        (const failed)
                    , notFound: const notFound
                    }
                    (const failed)
                _, _ -> failed
        pure Nothing

    -- The page reads the renewed post again, which the card's Renew keeps the
    -- focus through, and is back in search engines.
    let renewPost post = void $ Hooks.fork do
            renewed <- H.lift $ renew handle post
            case renewed of
                Nothing -> showToast { text: renewFailed, action: Nothing }
                Just text -> do
                    readPost
                    setMetaRobots "index, follow"
                    showToast { text, action: Nothing }

        feedSection game { page: { post, owner }, own, described } = let
            ownDescription = own # find (_.type >>> eq post.type) <#> _.description
            name = postName post
            title
                | isJust owner = "See what fits " <> name
                | otherwise = "More " <> game.title <> " posts"
            line
                | isJust owner && post.type == "player" = "Groups, communities and players that fit it come first."
                | isJust owner = "Players who fit it come first."
                | isJust described = "Posts that fit you come first."
                | otherwise = "Tell us about you, and the posts that fit come first."
            label
                | isJust owner = "See what fits"
                | isJust described = "See what fits you"
                | otherwise = "Browse " <> game.title <> " posts"
            in
            HH.section [ HS.class_ "post-feed", HPA.labelledBy "post-feed-title" ]
            [ HH.img [ HS.class_ "feed-cover", HP.src $ "/images/games/" <> game.handle <> ".webp", HP.alt "" ]
            , HH.div [ HS.class_ "post-feed-text" ] $ catMaybes
                [ Just $ HH.h2 [ HP.id "post-feed-title" ] [ HH.text title ]
                , Just $ HH.p_ [ HH.text line ]
                , if isJust owner then Nothing else described <#> \described' ->
                    HH.p [ HS.class_ "post-feed-described" ]
                    [ typeIcon described'.type, HH.span_ [ HH.text described'.text ] ]
                , Just $ HH.div [ HS.class_ "feed-active tabular" ]
                    [ HH.text $ show game.active <> " active " <> if game.active == 1 then "post" else "posts" ]
                ]
            -- See what fits takes the description from this post, as the home
            -- page's does (brief 11.2).
            , HH.a
                [ HS.class_ "button button-outline"
                , HP.href feedPath
                , HE.onClick \event -> do
                    when (isJust owner) $ liftEffect $
                        for_ ownDescription $ storeDescription handle post.type
                    navigateWithEvent_ feedPath event
                ]
                [ Icons.search, HH.text label ]
            ]

        notes { post, blocked, owner } = case blocked of
            Just "viewer" -> Just $ HH.p [ HS.class_ "post-note" ]
                [ Icons.ban
                , HH.span_
                    [ HH.text $ "You blocked " <> post.owner <> ", so there's no way to contact this post. "
                    , HH.a [ HP.href "/account#blocked", HE.onClick $ navigateWithEvent_ "/account#blocked" ]
                        [ HH.text "Unblock from your account" ]
                    , HH.text " to bring it back."
                    ]
                ]
            -- A block the owner made is theirs to know about.
            Just _ -> Just $ HH.p [ HS.class_ "post-note" ]
                [ Icons.ban, HH.span_ [ HH.text "There's no way to contact this post." ] ]
            Nothing
                | post.expired && not (isJust owner) -> Just $ HH.p [ HS.class_ "post-note" ]
                    [ Icons.info
                    , HH.span_
                        [ HH.text $ "This is an older post. " <> post.owner
                            <> " may no longer be looking, but you can still write."
                        ]
                    ]
                | otherwise -> Nothing

    Hooks.pure $ Ads.around case state.page, state.viewer of
        Missing, _ -> placeholder "Page could not be found."
        Failed, _ -> placeholder "There has been an error loading the post."
        Gone game, _ ->
            HH.div [ HS.class_ "post-page" ]
            [ HH.div [ HS.class_ "post-gone" ]
                [ HH.h1_ [ HH.text "This post is gone" ]
                , HH.p [ HS.class_ "muted" ]
                    [ HH.text $ "Whoever posted it deleted it. The " <> game.title
                        <> " feed has everyone else who is looking."
                    ]
                , HH.a [ HS.class_ "button button-primary", HP.href feedPath, HE.onClick $ navigateWithEvent_ feedPath ]
                    [ Icons.search, HH.text $ "Browse " <> game.title <> " posts" ]
                ]
            ]
        Shown shown@{ game, page }, Just viewer ->
            HH.div [ HS.class_ "post-page" ] $ catMaybes
            [ if feedBehind
                then Just $ HH.a
                    [ HS.class_ "button button-text button-small post-back"
                    , HP.href feedPath
                    , HE.onClick goBack
                    ]
                    [ Icons.arrowLeft, HH.text $ "Back to " <> game.title <> " posts" ]
                else Nothing
            , notes page
            , Just $ card
                { game
                , viewer
                , post: page.post
                , marked: false
                , expanded: true
                , place: Page
                    { blocked: isJust page.blocked
                    , status: case page.owner of
                        Just { expires, conversations, unread, conversation, reveals } ->
                            [ ownPostStatus
                                { now: viewer.now
                                , expires
                                , conversations
                                , unread
                                , conversation
                                , reveals
                                , onOpen: navigateWithEvent_
                                }
                            ]
                        Nothing -> []
                    }
                , onToggle: const $ pure unit
                , onContact: openPanel { signedIn: shown.signedIn, game } page.post
                , onEdit: navigate_ $ "/games/" <> handle <> "/post/" <> page.post.type <> "?from=edit"
                , onRenew: renewPost page.post
                }
            , Just $ feedSection game shown
            , panel <#> contactPanel viewer.now
            , Just $ toasts toast dismissToast
            ]
        _, _ -> HH.div [ HS.class_ "post-page" ] []

postPage :: ∀ action slots left.
    Int -> Input -> H.ComponentHTML action (postPage :: Slot__I Int | slots) (Async left)
postPage visit input = HH.slot_ (Proxy :: _ "postPage") visit component input
