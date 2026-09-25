module TeamTavern.Client.Pages.Feed (FeedCache, Input, feed) where

import Prelude

import Async (Async)
import Async as Async
import Control.Alt ((<|>))
import Data.Array (concatMap, elem, filter, find, foldl, head, index, null, snoc, sortBy)
import Data.Foldable (for_, traverse_)
import Data.Int (fromString, round)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks as Hooks
import Halogen.Subscription as Subscription
import TeamTavern.Client.Components.Ads as Ads
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button)
import TeamTavern.Client.Components.Card (Place(..), Viewer, card, tierOf, typeIcon)
import TeamTavern.Client.Components.ContactPanel (contactPanel, markMessaged, takeContactParam, useContactPanel)
import TeamTavern.Client.Components.Divider (divider, tierHeading)
import TeamTavern.Client.Components.Overlay (Presentation(..), useOverlay)
import TeamTavern.Client.Components.Toast (toasts, useToast)
import TeamTavern.Client.Components.UsePhone (usePhone)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Pages.Feed.Bar (bar, sheet, summaryButton)
import TeamTavern.Client.Pages.Feed.Description (Stored, current, describes, emptyDescription, emptyStored, isEmpty, loadStored, saveStored, setCurrent, storedFrom)
import TeamTavern.Client.Pages.Feed.Fields (Lists, barFields)
import TeamTavern.Client.Pages.Placeholder (placeholder)
import TeamTavern.Client.Script.Expand (toggleCard)
import TeamTavern.Client.Script.Focus (focusSoon)
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_, navigate_)
import TeamTavern.Client.Script.QueryParams (getQueryParam, removeQueryParam)
import TeamTavern.Client.Script.RenderReady (appendRenderReadyNotFound, appendRenderReadyUnavailable)
import TeamTavern.Client.Script.Scroll (onScroll)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Shared.Fetch (fetchPath, fetchPathBody, fetchSimple)
import TeamTavern.Client.Shared.Me (fetchMe)
import TeamTavern.Client.Shared.Renew (renew, renewFailed)
import TeamTavern.Client.Shared.Slot (Slot__I)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Country.ViewCountries (ViewCountries)
import TeamTavern.Routes.Country.ViewCountries as ViewCountries
import TeamTavern.Routes.Feed.ViewFeed (ViewFeed)
import TeamTavern.Routes.Feed.ViewFeed as ViewFeed
import TeamTavern.Routes.Feed.ViewOwnDescriptions (OwnDescription, ViewOwnDescriptions)
import TeamTavern.Routes.Game.ViewGame (ViewGame)
import TeamTavern.Routes.Game.ViewGame as ViewGame
import TeamTavern.Routes.Post.ViewPost (ViewPost)
import TeamTavern.Routes.Shared.Card (CardRow)
import TeamTavern.Shared.Languages (allLanguages)
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Location (pathname)
import Web.HTML.Window (location, scroll)
import Web.UIEvent.MouseEvent (MouseEvent)

-- | What the feed had loaded, put back when the browser returns to it
-- | (brief 11.1): the description, the batches, the cards opened and where
-- | the page was scrolled to, with everything else its first render draws, so
-- | the page is as tall as it was left before anything is fetched again.
type FeedCache =
    { game :: ViewGame.OkContent
    , viewer :: Viewer
    , own :: Array OwnDescription
    , stored :: Stored
    , segment :: String
    , feed :: ViewFeed.OkContent
    , expanded :: Array Int
    , y :: Number
    }

-- | `restore` is the feed as it was left, given only when the browser went
-- | back or forward to it; `cache` is where the feed keeps itself for that.
type Input =
    { handle :: String
    , restore :: Maybe FeedCache
    , cache :: Ref (Map String FeedCache)
    }

data Game = Loading | Loaded ViewGame.OkContent | Missing | Failed

type State =
    { game :: Game
    , countries :: ViewCountries.OkContent
    , own :: Array OwnDescription
    , nickname :: Maybe String
    , stored :: Stored
    , segment :: String
    , feed :: Maybe ViewFeed.OkContent
    -- The batch asked for last hasn't answered.
    , busy :: Boolean
    , loadingMore :: Boolean
    , failed :: Boolean
    , expanded :: Array Int
    , openField :: Maybe String
    , showMore :: Boolean
    , sheetOpen :: Boolean
    , viewer :: Maybe Viewer
    -- The note a renewal link lands under, until the description changes.
    , renewed :: Maybe String
    }

segments :: Array { value :: String, label :: String }
segments =
    [ { value: "all", label: "All" }
    , { value: "player", label: "Players" }
    , { value: "group", label: "Groups" }
    , { value: "community", label: "Communities" }
    ]

olderPosts :: String
olderPosts = "Older posts · they may no longer be looking"

-- Signed out, the note names whose post the description came from (brief 9).
renewedNote :: CardRow -> String
renewedNote post =
    subject <> " is active again for " <> days <> " days. Showing what fits it."
    where
    subject
        | post.own = "Your " <> post.type <> " post"
        | otherwise = fromMaybe (post.owner <> "'s " <> post.type <> " post") post.name
    days = if post.type == "community" then "90" else "30"

-- The languages the loaded posts use, most used first, then every other.
languagesByUse :: Array CardRow -> Array String
languagesByUse posts = used <> filter (not <<< flip elem used) allLanguages
    where
    used =
        posts
        # concatMap _.languages
        # foldl (\counts language -> Map.insertWith (+) language 1 counts) Map.empty
        # (Map.toUnfoldable :: _ -> Array (Tuple String Int))
        # sortBy (\a b -> compare (snd b) (snd a) <> compare (fst a) (fst b))
        <#> fst

component :: ∀ query output left. H.Component query Input output (Async left)
component = Hooks.component \_ { handle, restore, cache } -> Hooks.do
    state /\ stateId <- Hooks.useState
        { game: maybe Loading (Loaded <<< _.game) restore
        , countries: { regions: [], countries: [] }
        , own: maybe [] _.own restore
        , nickname: Nothing
        , stored: maybe emptyStored _.stored restore
        , segment: maybe "all" _.segment restore
        , feed: restore <#> _.feed
        , busy: isNothing restore
        , loadingMore: false
        , failed: false
        , expanded: maybe [] _.expanded restore
        , openField: Nothing
        , showMore: false
        , sheetOpen: false
        , viewer: restore <#> _.viewer
        , renewed: Nothing
        }
    _ /\ requestRef <- Hooks.useRef 0
    phone <- usePhone
    { toast, showToast, dismissToast } <- useToast

    let popoverRef = H.RefLabel "feed-popover"
        sheetRef = H.RefLabel "feed-sheet"

        -- Every change is kept for Back, which puts the feed back as it was.
        update f = do
            state' <- Hooks.modify stateId f
            case state'.game, state'.viewer, state'.feed of
                Loaded game, Just viewer, Just feed' -> liftEffect $ Ref.modify_
                    (Map.alter
                        (\entry -> Just
                            { game
                            , viewer
                            , own: state'.own
                            , stored: state'.stored
                            , segment: state'.segment
                            , feed: feed'
                            , expanded: state'.expanded
                            , y: maybe 0.0 _.y entry
                            })
                        handle)
                    cache
                _, _, _ -> pure unit

        -- A batch after the cursor, or the first. Only the batch asked for
        -- last is shown, whatever order the answers come back in.
        load cursor = do
            request <- liftEffect $ Ref.modify (_ + 1) requestRef
            state' <- Hooks.modify stateId _ { busy = true }
            timezone <- getClientTimezone
            let description = current state'.stored
            result <- H.lift $ Async.attempt $ fetchPathBody (Proxy :: _ ViewFeed) { handle }
                { description: description { timezone = description.timezone <|> Just timezone }
                , showing: if state'.segment == "all" then [] else [ state'.segment ]
                , cursor
                }
            latest <- liftEffect $ Ref.read requestRef
            let failed = do
                    when (isNothing state'.feed) appendRenderReadyUnavailable
                    update _ { failed = true, loadingMore = false, busy = false }
            when (latest == request) case result of
                Right response -> response # onMatch
                    { ok: \batch -> update \state'' -> state''
                        { feed = Just case cursor, state''.feed of
                            Just _, Just loaded -> batch { posts = loaded.posts <> batch.posts }
                            _, _ -> batch
                        , failed = false
                        , loadingMore = false
                        , busy = false
                        }
                    -- The game's own lookup says it isn't found.
                    , notFound: const $ Hooks.modify_ stateId _ { busy = false }
                    }
                    (const failed)
                Left _ -> failed

        reload = void $ Hooks.fork $ load Nothing

    -- A block takes the owner's posts out of the feed, and Undo puts them back.
    { panel, openPanel, openPanelById } <- useContactPanel
        { onMessaged: \id time -> Hooks.modify_ stateId \state' -> state'
            { feed = state'.feed <#> \feed' -> feed' { posts = markMessaged id time <$> feed'.posts } }
        , onBlockChange: reload
        , showToast
        }

    let loadMore = do
            state' <- Hooks.get stateId
            unless state'.loadingMore do
                update _ { loadingMore = true }
                void $ Hooks.fork $ load $ state'.feed >>= _.cursor

        -- The description follows every change on a desktop; on a phone the
        -- feed waits for the sheet to close (brief 7.1).
        change stored = do
            state' <- Hooks.modify stateId _ { stored = stored, renewed = Nothing }
            liftEffect $ saveStored handle stored
            unless state'.sheetOpen reload

        changeDescription description = do
            state' <- Hooks.get stateId
            change $ setCurrent description state'.stored

        changeType type_ = do
            state' <- Hooks.get stateId
            update _ { segment = "all", openField = Nothing, showMore = false }
            change state'.stored { type = type_ }

        closeSheet = do
            update _ { sheetOpen = false }
            reload

    useOverlay popoverRef (Dropdown { className: "popover", role: "dialog" })
        (isJust state.openField && not phone) (update _ { openField = Nothing })
    useOverlay sheetRef FullScreen (state.sheetOpen && phone) closeSheet

    -- Crossing the breakpoint closes whatever the other layout had open.
    Hooks.captures { phone } Hooks.useTickEffect do
        state' <- Hooks.get stateId
        update _ { openField = Nothing }
        when state'.sheetOpen closeSheet
        pure Nothing

    Hooks.useLifecycleEffect do
        now' <- liftEffect now
        timezone <- getClientTimezone
        -- A feed put back reads the time afresh for how long ago each post was.
        update _ { viewer = Just { now: now', timezone } }
        -- Opened afresh, the feed starts over, and so does what Back restores.
        when (isNothing restore) $ liftEffect $ Ref.modify_ (Map.delete handle) cache

        void $ Hooks.fork do
            result <- H.lift $ Async.attempt $ fetchPath (Proxy :: _ ViewGame) { handle }
            case result of
                Right response -> response # onMatch
                    { ok: \game -> do
                        setMeta (game.title <> " LFG and team finder | TeamTavern")
                            ( game.description # head # fromMaybe
                                ("Find " <> game.title <> " players, groups and communities on TeamTavern.")
                            )
                        update _ { game = Loaded game }
                        -- Back from signing up to contact a post, its panel opens.
                        takeContactParam >>= traverse_ (openPanelById game)
                    , notFound: const do
                        appendRenderReadyNotFound
                        setMeta "Page not found | TeamTavern" ""
                        Hooks.modify_ stateId _ { game = Missing }
                    }
                    (const $ appendRenderReadyUnavailable *> Hooks.modify_ stateId _ { game = Failed })
                Left _ -> appendRenderReadyUnavailable *> Hooks.modify_ stateId _ { game = Failed }

        -- Landed on from a renewal link, whose page stored the post's description.
        void $ Hooks.fork do
            renewedId <- getQueryParam "renewed" <#> (_ >>= fromString)
            for_ renewedId \id -> do
                removeQueryParam "renewed"
                result <- H.lift $ Async.attempt $ fetchPath (Proxy :: _ ViewPost) { handle, id }
                case result of
                    Right response -> response # onMatch
                        { ok: \{ post } -> Hooks.modify_ stateId _ { renewed = Just $ renewedNote post } }
                        (const $ pure unit)
                    Left _ -> pure unit

        void $ Hooks.fork do
            result <- H.lift $ Async.attempt $ fetchSimple (Proxy :: _ ViewCountries)
            case result of
                Right response -> response # onMatch
                    { ok: \countries -> Hooks.modify_ stateId _ { countries = countries } }
                    (const $ pure unit)
                Left _ -> pure unit

        -- With nothing described for the game yet, the description starts
        -- from the viewer's own post in it (brief 7.1), unsaved until changed.
        void $ Hooks.fork do
            me <- H.lift fetchMe
            own <- if isNothing me then pure [] else do
                result <- H.lift $ Async.attempt $ fetchPath (Proxy :: _ ViewOwnDescriptions) { handle }
                pure case result of
                    Right response -> response # onMatch { ok: identity } (const [])
                    Left _ -> []
            update _ { own = own, nickname = me <#> _.nickname }
            when (isNothing restore) do
                stored <- liftEffect $ loadStored handle
                Hooks.modify_ stateId _ { stored = stored <|> storedFrom own # fromMaybe emptyStored }
                load Nothing

        -- The first render drew the feed as it was left, so it can be
        -- scrolled to where it was at once.
        for_ restore \{ y } -> liftEffect $ window >>= scroll 0 (round y)
        -- Leaving by a link scrolls the next page to the top once the location
        -- is already its, which is not where the feed was left.
        subscription <- Hooks.subscribe $ Subscription.makeEmitter onScroll <#> \y -> liftEffect do
            path <- window >>= location >>= pathname
            when (path == "/games/" <> handle) $
                Ref.modify_ (Map.update (\entry -> Just entry { y = y }) handle) cache
        pure $ Just $ Hooks.unsubscribe subscription

    let description = current state.stored
        empty = isEmpty description

        publishLink type_ label =
            HH.a
            [ HS.class_ "button button-primary"
            , HP.href path
            , HE.onClick \event -> do
                -- The post screen reads the description from storage,
                -- however it was set.
                liftEffect $ saveStored handle state.stored
                navigateWithEvent_ path event
            ]
            [ HH.text label ]
            where
            path = "/games/" <> handle <> "/post/" <> type_ <> "?from=feed"

        prompt icon quiet content =
            HH.div [ HS.class_ $ "publish-prompt" <> if quiet then " publish-prompt-quiet" else "" ]
            ([ icon, HH.p_ [ HH.text content.text ] ] <> maybe [] pure content.action)

        -- A viewer with a post of the type isn't asked to publish another
        -- (brief 7.1). While the description says what the post says, the
        -- feed says whose post it is showing what fits; once it differs, it
        -- offers to update the post.
        publishPrompt game = case find (_.type >>> eq description.type) state.own of
            Just own
                | empty -> HH.text ""
                | describes own.description description ->
                    prompt (typeIcon own.type) true
                        { text: "Showing what fits " <> ownName own true <> ".", action: Nothing }
                | otherwise ->
                    prompt Icons.megaphone false
                        { text: "Update " <> ownName own false <> " with this: "
                            <> (if own.type == "player" then "groups and players" else "players")
                            <> " who fit it find you, and we'll tell you when someone new does."
                        , action: Just $ publishLink own.type "Update post"
                        }
            Nothing
                | empty && game.active > 0 -> HH.text ""
                | otherwise ->
                    prompt Icons.megaphone false
                        { text:
                            if empty
                            then "Nobody has posted for " <> game.title
                                <> " lately. Publish your post and we'll tell you when someone fits."
                            else "Publish this as your post: "
                                <> (if description.type == "player" then "groups and players" else "players")
                                <> " can find you too, and we'll tell you when someone new fits."
                        , action: Just $ publishLink description.type "Publish post"
                        }

        ownName own quiet =
            if own.type == "player" then "your player post"
            else
                let name = own.name <|> (state.nickname <#> \nickname -> nickname <> "'s " <> own.type)
                        # fromMaybe ("your " <> own.type)
                in if quiet then name <> ", your " <> own.type <> " post" else name

        segmentButtons =
            if description.type /= "player" then HH.text ""
            else HH.div_ $ pure $ HH.div [ HS.class_ "segments", HPA.role "group", HPA.label "Showing" ] $
                segments <#> \{ value, label } ->
                    HH.button
                    [ HS.class_ "segment"
                    , HP.type_ HP.ButtonButton
                    , HPA.pressed $ show $ state.segment == value
                    , HE.onClick $ const do
                        update _ { segment = value }
                        reload
                    ]
                    [ HH.text label ]

        cardOf game viewer post = card
            { game
            , viewer
            , post
            , marked: not empty
            , expanded: elem post.id state.expanded
            , place: Listed
            , onToggle: \(event :: MouseEvent) -> toggleCardOf event post.id
            , onContact: openPanel { signedIn: isJust state.nickname, game } post
            , onEdit: navigate_ $ "/games/" <> handle <> "/post/" <> post.type <> "?from=edit"
            , onRenew: renewPost post
            }

        -- A renewed post is active again, so the feed is asked again for
        -- where it now stands.
        renewPost post = void $ Hooks.fork do
            renewed <- H.lift $ renew handle post
            case renewed of
                Nothing -> showToast { text: renewFailed, action: Nothing }
                Just text -> do
                    load Nothing
                    showToast { text, action: Nothing }

        toggleCardOf event id = toggleCard event $ update \state' -> state'
            { expanded = if elem id state'.expanded then filter (notEq id) state'.expanded else snoc state'.expanded id }

        -- Active posts in tiers, then the divider, then expired posts in the
        -- same order without headings (brief 4 and 7.2).
        posts game viewer { posts: rows, tiers } = let
            counts = [ tiers.fits, tiers.missingOne, tiers.missingMore ]
            headings = [ "Fits you", "Missing one thing", "Missing more" ]
            flush acc = if null acc.stack then acc else acc
                { html = snoc acc.html (HK.div [ HS.class_ "feed-stack" ] acc.stack), stack = [] }
            step acc post = let
                acc' =
                    if post.expired && not acc.expired
                    then (flush acc) { html = snoc (flush acc).html (divider olderPosts), expired = true }
                    else acc
                tier = tierOf post
                acc'' =
                    if not empty && not post.expired && Just tier /= acc'.tier
                    then (flush acc')
                        { html = snoc (flush acc').html
                            (tierHeading (fromMaybe "" $ headingAt tier) (countAt tier))
                        , tier = Just tier
                        }
                    else acc'
                in
                -- Keyed, so a card's element stays with its post as the order changes.
                acc'' { stack = snoc acc''.stack (Tuple (show post.id) (cardOf game viewer post)) }
            headingAt tier = index headings tier
            countAt tier = index counts tier
            in
            (flush $ foldl step { html: [], stack: [], tier: Nothing, expired: false } rows).html

    Hooks.pure $ Ads.around case state.game of
        Missing -> placeholder "Page could not be found."
        Failed -> placeholder "There has been an error loading the game."
        Loading -> HH.div [ HS.class_ "feed-page" ] []
        Loaded game -> let
            lists :: Lists
            lists =
                { countries: state.countries.countries <#> _.name
                , regions: state.countries.regions
                , languages: languagesByUse (maybe [] _.posts state.feed)
                }
            fields = barFields game lists description.type
            in
            HH.div [ HS.class_ "feed-page" ]
            [ HH.div [ HS.class_ "feed-header" ]
                [ HH.img [ HS.class_ "feed-cover", HP.src $ "/images/games/" <> game.handle <> ".webp", HP.alt "" ]
                , HH.div_
                    [ HH.h1_ [ HH.text $ game.title <> " LFG" ]
                    , HH.p_ [ HH.text "Find players, groups and communities" ]
                    , HH.div [ HS.class_ "feed-active tabular" ]
                        [ HH.text $ show game.active <> " active " <> if game.active == 1 then "post" else "posts" ]
                    ]
                ]
            , if phone
                then summaryButton { fields, description, onOpen: update _ { sheetOpen = true } }
                else bar
                    { ref: popoverRef
                    , fields
                    , description
                    , openField: state.openField
                    , showMore: state.showMore
                    , onType: changeType
                    , onChange: changeDescription
                    , onOpen: \field -> update _ { openField = field }
                    -- More and Clear all go once pressed, and hand the focus on
                    -- to the chips they leave.
                    , onMore: do
                        update _ { showMore = true }
                        liftEffect $ focusSoon "[data-first-more]"
                    , onClearAll: do
                        change $ setCurrent (emptyDescription description.type) state.stored
                        liftEffect $ focusSoon ".field-chips .field-chip"
                    }
            , case state.renewed of
                Just text -> prompt Icons.refreshCw true { text, action: Nothing }
                Nothing -> publishPrompt game
            , segmentButtons
            , case state.feed, state.viewer of
                Nothing, _ | state.failed -> HH.p_ [ HH.text "There has been an error loading the posts." ]
                feed', viewer ->
                    HH.div [ HS.class_ "feed", HPA.busy $ show state.busy ] $
                        fromMaybe [] $ posts game <$> viewer <*> feed'
            , case state.feed of
                Just { more: true } ->
                    HH.div [ HS.class_ "load-more" ]
                    [ button Outline Regular loadMore [ HH.text "Load more" ] ]
                _ -> HH.text ""
            , if phone && state.sheetOpen
                then sheet
                    { ref: sheetRef
                    , fields
                    , description
                    , onType: changeType
                    , onChange: changeDescription
                    , onClose: closeSheet
                    }
                else HH.text ""
            , case panel, state.viewer of
                Just panel', Just viewer -> contactPanel viewer.now panel'
                _, _ -> HH.text ""
            , toasts toast dismissToast
            ]

feed :: ∀ action slots left.
    Int -> Input -> H.ComponentHTML action (feed :: Slot__I Int | slots) (Async left)
feed visit input = HH.slot_ (Proxy :: _ "feed") visit component input
