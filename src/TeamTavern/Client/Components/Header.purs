module TeamTavern.Client.Components.Header (header) where

import Prelude

import Async (Async)
import Async as Async
import Data.Array (find)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String (Pattern(..), split, stripPrefix, take, toUpper)
import Data.Tuple.Nested ((/\))
import Data.Variant (onMatch)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Halogen.Hooks as Hooks
import Halogen.Subscription as Subscription
import TeamTavern.Client.Components.CoverGrid (coverGrid, feedPath)
import TeamTavern.Client.Components.Menu (menuDivider, menuItem, menuLabel, menuLink, sheetMenu)
import TeamTavern.Client.Components.Notifications (notificationPath, notifications)
import TeamTavern.Client.Components.Overlay (Presentation(..), overlay, useOverlay)
import TeamTavern.Client.Components.Unread (badge)
import TeamTavern.Client.Components.UsePhone (usePhone)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Script.Back (authPath, currentBack)
import TeamTavern.Client.Script.Focus (focusSoon)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_, navigate_)
import TeamTavern.Client.Script.Unread (onUnread)
import TeamTavern.Client.Shared.Fetch (fetchPathNoContent, fetchSimple)
import TeamTavern.Client.Shared.Me (fetchMe)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Game.ViewGames (ViewGames)
import TeamTavern.Routes.Game.ViewGames as ViewGames
import TeamTavern.Routes.Notification.ReadNotification (ReadNotification)
import TeamTavern.Routes.Notification.ReadNotifications (ReadNotifications)
import TeamTavern.Routes.Notification.ViewNotifications (ViewNotifications)
import TeamTavern.Routes.Notification.ViewNotifications as ViewNotifications
import TeamTavern.Routes.Player.ViewMe (ViewMe)
import TeamTavern.Routes.Player.ViewMe as ViewMe
import TeamTavern.Routes.Session.EndSession (EndSession)
import Type.Proxy (Proxy(..))

-- One menu is open at a time: Games, notifications, the account menu, or, on
-- a phone signed out, the menu holding Sign in and Sign up.
data Menu = Games | Notifications | AccountMenu | SignedOutMenu

derive instance Eq Menu

-- Who the header shows, which only the server can say. Until it first has,
-- the header shows neither the account nor Sign in.
data Viewer = Unknown | SignedOut | SignedIn ViewMe.OkContent

type State =
    { viewer :: Viewer
    , visit :: Int
    , games :: Array ViewGames.OkGameContent
    , back :: String
    , menu :: Maybe Menu
    , notifications :: Maybe { now :: Instant, list :: Array ViewNotifications.Notification }
    }

sameViewer :: Viewer -> Viewer -> Boolean
sameViewer (SignedIn one) (SignedIn other) = one.nickname == other.nickname
sameViewer _ _ = false

ref :: Menu -> H.RefLabel
ref Games = H.RefLabel "header-games"
ref Notifications = H.RefLabel "header-notifications"
ref AccountMenu = H.RefLabel "header-account"
ref SignedOutMenu = H.RefLabel "header-menu"

-- On a desktop a menu hangs from its button. On a phone Games and
-- notifications take the whole screen, like the feed's description, and the
-- menus of a few rows are a sheet from the bottom.
presentation :: Boolean -> Menu -> Presentation
presentation true Games = FullScreen
presentation true Notifications = FullScreen
presentation true _ = Bottom
presentation false Games =
    Dropdown { className: "header-dropdown header-dropdown-games header-menu", role: "dialog" }
presentation false Notifications =
    Dropdown { className: "header-dropdown header-dropdown-notifications header-menu", role: "dialog" }
presentation false _ = Dropdown { className: "menu header-menu", role: "menu" }

-- The header doesn't know which post type is being looked for, so it marks the
-- game rather than a post.
postsMark :: Int -> Maybe String
postsMark 0 = Nothing
postsMark 1 = Just "Your post"
postsMark _ = Just "Your posts"

-- The router counts the visits, so even a link to the page already open closes
-- the menu it was in.
type Input = { path :: String, visit :: Int }

component :: ∀ query output left. H.Component query Input output (Async left)
component = Hooks.component \_ { path, visit } -> Hooks.do
    phone <- usePhone
    state /\ stateId <- Hooks.useState
        ({ viewer: Unknown, visit, games: [], back: "/", menu: Nothing, notifications: Nothing } :: State)

    let set = Hooks.modify_ stateId
        close = set _ { menu = Nothing }
        isOpen menu = state.menu == Just menu

        -- The counts are asked for again once something has been read.
        -- Only a player signed in reads anything.
        refreshMe = do
            me <- H.lift fetchMe
            for_ me \me' -> set \state' -> case state'.viewer of
                SignedIn _ -> state' { viewer = SignedIn me' }
                _ -> state'

        -- The list is asked for each time it opens, and shows what it last
        -- held meanwhile.
        loadNotifications = do
            result <- H.lift $ Async.attempt $ fetchSimple (Proxy :: _ ViewNotifications)
            now' <- liftEffect now
            case result of
                Right response -> response # onMatch
                    { ok: \list -> set _ { notifications = Just { now: now', list } } }
                    (const $ pure unit)
                Left _ -> pure unit

        toggle menu
            | isOpen menu = close
            | otherwise = do
                set _ { menu = Just menu }
                when (menu == Notifications) $ void $ Hooks.fork loadNotifications

        markRead read = set \state' -> state'
            { notifications = state'.notifications <#> \loaded ->
                loaded { list = loaded.list <#> \notification ->
                    if read notification then notification { read = true } else notification }
            }

        -- Opening a notification reads it (brief 11.3).
        openNotification notification event = do
            unless notification.read do
                markRead (_.id >>> eq notification.id)
                void $ Hooks.fork do
                    void $ H.lift $ fetchPathNoContent (Proxy :: _ ReadNotification) { id: notification.id }
                    refreshMe
            navigateWithEvent_ (notificationPath notification) event

        -- The list stays open with every row read. Mark all read goes with the
        -- last unread row, so the focus moves to the first row.
        readAll = do
            markRead $ const true
            liftEffect $ focusSoon ".site-header-root .notification"
            void $ Hooks.fork do
                void $ H.lift $ Async.attempt $ fetchSimple (Proxy :: _ ReadNotifications)
                refreshMe

    -- A page that reads a conversation or sends a message says so, and the
    -- count is asked for again.
    Hooks.useLifecycleEffect do
        void $ Hooks.fork do
            result <- H.lift $ Async.attempt $ fetchSimple (Proxy :: _ ViewGames)
            case result of
                Right response -> response # onMatch { ok: \games -> set _ { games = games } } (const $ pure unit)
                Left _ -> pure unit
        subscription <- Hooks.subscribe $ Subscription.makeEmitter (\emit -> onUnread (emit unit)) <#> \_ ->
            void $ Hooks.fork refreshMe
        pure $ Just $ Hooks.unsubscribe subscription

    -- Every visit asks the server afresh, so signing in or out shows on the
    -- next page. The header keeps showing what it last knew meanwhile, and
    -- takes an answer only while its visit is the latest. The server is asked
    -- on a fork: the header renders nothing new until its effects are done,
    -- and a menu opened meanwhile has to open. Another player's notifications
    -- are not kept to show meanwhile.
    Hooks.captures { visit } Hooks.useTickEffect do
        back <- currentBack
        set _ { back = back, menu = Nothing, visit = visit }
        void $ Hooks.fork do
            result <- H.lift $ Async.attempt $ fetchSimple (Proxy :: _ ViewMe)
            let answer viewer = set \state' ->
                    if state'.visit /= visit then state'
                    else state'
                        { viewer = viewer
                        , notifications = if sameViewer state'.viewer viewer then state'.notifications else Nothing
                        }
            case result of
                Right response -> response # onMatch
                    { ok: answer <<< SignedIn
                    , notAuthorized: const $ answer SignedOut
                    }
                    (const $ pure unit)
                Left _ -> pure unit
        pure Nothing

    -- A menu drawn for one width has no place at the other.
    Hooks.captures { phone } Hooks.useTickEffect do
        close
        pure Nothing

    useOverlay (ref Games) (presentation phone Games) (isOpen Games) close
    useOverlay (ref Notifications) (presentation phone Notifications) (isOpen Notifications) close
    useOverlay (ref AccountMenu) (presentation phone AccountMenu) (isOpen AccountMenu) close
    useOverlay (ref SignedOutMenu) (presentation phone SignedOutMenu) (isOpen SignedOutMenu) close

    let -- Signing out lands on the home page: the page the player was on may
        -- have been theirs, and signed out the home page is what the site is for.
        signOut = do
            void $ H.lift $ Async.attempt $ fetchSimple (Proxy :: _ EndSession)
            set _ { viewer = SignedOut, menu = Nothing, notifications = Nothing }
            navigate_ "/"

        link class_ path' =
            HH.a [ HS.class_ class_, HP.href path', HE.onClick $ navigateWithEvent_ path' ]

        title Games = "Games"
        title Notifications = "Notifications"
        title AccountMenu = case state.viewer of
            SignedIn me -> me.nickname
            _ -> ""
        title SignedOutMenu = "Menu"

        postsIn handle = case state.viewer of
            SignedIn me -> me.games # find (_.handle >>> eq handle) # maybe 0 _.posts
            _ -> 0

        items AccountMenu =
            [ menuLink "/" [ HH.text "Your posts" ]
            , menuLink "/account" [ HH.text "Account" ]
            , menuDivider
            , menuItem signOut [ HH.text "Sign out" ]
            ]
        items _ =
            [ menuLink (authPath "/signin" state.back) [ HH.text "Sign in" ]
            , menuLink (authPath "/signup" state.back) [ HH.text "Sign up" ]
            ]

        body Games = [ coverGrid { games: state.games, href: feedPath, mark: postsIn >>> postsMark } ]
        body Notifications = notifications
            { phone, loaded: state.notifications, onOpen: openNotification, onReadAll: readAll }
        body menu
            | phone = [ sheetMenu $ items menu ]
            | otherwise = [ menuLabel $ title menu ] <> items menu

        menuOverlay menu =
            overlay { ref: ref menu, presentation: presentation phone menu, title: title menu, onClose: close }
            (body menu) []

        -- A dropdown sits beside the button it hangs from; a phone's overlay
        -- hangs from nothing and sits after the header, outside its stacking
        -- context.
        dropdown menu = if isOpen menu && not phone then menuOverlay menu else HH.text ""

        opener menu class_ popup label content =
            HH.div [ HS.class_ class_ ]
            [ HH.button
                ( [ HS.class_ $ case menu of
                        Games -> "button button-text"
                        AccountMenu -> "avatar"
                        Notifications -> "icon-button header-count"
                        SignedOutMenu -> "icon-button"
                  , HP.type_ HP.ButtonButton
                  , HPA.hasPopup popup
                  , HPA.expanded $ show $ isOpen menu
                  , HE.onClick $ const $ toggle menu
                  ]
                  <> maybe [] (\label' -> [ HPA.label label' ]) label
                )
                content
            , dropdown menu
            ]

        counted label word n = label <> if n > 0 then ", " <> show n <> " " <> word else ""

        -- On a game's pages, New post knows the game (brief 6).
        newPostPath = case Array.take 3 $ split (Pattern "/") path of
            [ "", "games", handle ] -> "/post?game=" <> handle
            _ -> "/post"

        newPost signedIn =
            HH.a
            [ HS.class_ "button button-outline button-small"
            , HP.href newPostPath
            , HPA.label "New post"
            , HE.onClick $ navigateWithEvent_ newPostPath
            ]
            [ Icons.plus
            , HH.span (if signedIn then [ HS.class_ "hide-phone" ] else []) [ HH.text "New post" ]
            ]

        signedInActions me =
            [ newPost true
            , HH.a
                ( [ HS.class_ "icon-button header-count"
                  , HP.href "/messages"
                  , HPA.label $ counted "Messages" "unread" me.unreadConversations
                  , HE.onClick $ navigateWithEvent_ "/messages"
                  ]
                  <> if isJust (stripPrefix (Pattern "/messages") path)
                    then [ HP.attr (HH.AttrName "aria-current") "page" ] else []
                )
                ([ Icons.mail ] <> if me.unreadConversations > 0 then [ badge me.unreadConversations ] else [])
            , opener Notifications "header-wrap" "dialog"
                (Just $ counted "Notifications" "new" me.unreadNotifications)
                ([ Icons.bell ] <> if me.unreadNotifications > 0 then [ badge me.unreadNotifications ] else [])
            , opener AccountMenu "header-wrap" "menu" (Just "Account menu")
                [ HH.text $ toUpper $ take 1 me.nickname ]
            ]

        signedOutActions =
            [ newPost false
            , link "button button-text button-small hide-phone" (authPath "/signin" state.back) [ HH.text "Sign in" ]
            , link "button button-text button-small hide-phone" (authPath "/signup" state.back) [ HH.text "Sign up" ]
            , opener SignedOutMenu "header-wrap show-phone" "menu" (Just "Menu") [ Icons.menu ]
            ]

    Hooks.pure $
        HH.div [ HS.class_ "site-header-root" ]
        [ HH.header [ HS.class_ "site-header" ]
            [ HH.div [ HS.class_ "site-header-inner" ]
                [ HH.a [ HS.class_ "logo", HP.href "/", HPA.label "TeamTavern", HE.onClick $ navigateWithEvent_ "/" ]
                    [ HH.img [ HS.class_ "logo-mark", HP.src "/logo-mark.svg", HP.alt "" ]
                    , HH.span [ HS.class_ "logo-word" ] [ HH.text "TeamTavern" ]
                    ]
                , opener Games "header-wrap" "dialog" Nothing [ HH.text "Games", Icons.chevronDown ]
                , HH.div [ HS.class_ "site-header-actions" ] case state.viewer of
                    Unknown -> []
                    SignedOut -> signedOutActions
                    SignedIn me -> signedInActions me
                ]
            ]
        , case state.menu of
            Just menu | phone -> menuOverlay menu
            _ -> HH.text ""
        ]

header :: ∀ action slots left. Input -> H.ComponentHTML action (header :: Slot___ | slots) (Async left)
header = HH.slot_ (Proxy :: _ "header") unit component
