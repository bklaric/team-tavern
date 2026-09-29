module TeamTavern.Client.Components.Notifications (notificationPath, notifications) where

import Prelude

import Data.Array (any, findIndex, modifyAt, partition, snoc)
import Data.DateTime.Instant (Instant)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button, buttonLink)
import TeamTavern.Client.Components.InboxRow (inboxPostName)
import TeamTavern.Client.Components.OwnPostStatus (termWords)
import TeamTavern.Client.Components.Unread (unreadDot)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Script.Ago (ago)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Notification.ViewNotifications (Notification, NotificationPost)
import Web.UIEvent.MouseEvent (MouseEvent)

type Group = { post :: NotificationPost, rows :: Array Notification }

-- Each of the player's own posts holds the notifications about it, and the
-- post with the newest one leads, since the list comes newest first. Inside a
-- post its own expiry comes first, as the row with something to do, and the
-- posts that fit follow, newest first (brief 11.3).
groups :: Array Notification -> Array Group
groups = foldl add [] >>> map \group -> group { rows = expiryFirst group.rows }
    where
    add groups' notification =
        case findIndex (_.post.id >>> eq notification.post.id) groups' of
            Just index -> fromMaybe groups' $ modifyAt index (\group -> group { rows = snoc group.rows notification }) groups'
            Nothing -> snoc groups' { post: notification.post, rows: [ notification ] }
    expiryFirst rows = let { yes, no } = partition (_.kind >>> eq "expiry") rows in yes <> no

-- | A post that fits opens its own page; a post of the player's about to expire
-- | opens their posts, where it is renewed.
notificationPath :: Notification -> String
notificationPath notification = case notification.fitting of
    Just fitting -> "/games/" <> notification.post.handle <> "/posts/" <> show fitting.id
    Nothing -> "/"

typeWord :: String -> String
typeWord "group" = "Group"
typeWord "community" = "Community"
typeWord _ = "Player"

-- A post that fits is named with its type and the time it was published; an
-- expiry reads the post's state now, in the home page's words, and where to
-- act on it.
row :: ∀ w m. Instant -> (Notification -> MouseEvent -> m Unit) -> Notification -> HH.HTML w (m Unit)
row now onOpen notification = let
    { icon, title, meta } = case notification.fitting of
        Just fitting ->
            { icon: Nothing
            , title: inboxPostName fitting <> " fits"
            , meta: typeWord fitting.type <> " · " <> ago now notification.created
            }
        Nothing -> let term = termWords now notification.post.expires in
            { icon: Just term.icon, title: term.text, meta: "Renew it from your posts." }
    in
    HH.a
    [ HS.class_ if notification.read then "notification" else "notification notification-unread"
    , HP.href $ notificationPath notification
    , HE.onClick $ onOpen notification
    ]
    [ HH.span [ HS.class_ "notification-mark" ] if notification.read then [] else unreadDot
    , HH.span [ HS.class_ "notification-main" ]
        [ HH.span [ HS.class_ "notification-title" ] $ maybe [] pure icon <> [ HH.text title ]
        , HH.span [ HS.class_ "notification-meta" ] [ HH.text meta ]
        ]
    ]

-- | The bell's list (brief 11.3), what the overlay holding it shows. Mark all
-- | read shows only while something is unread: in the heading row on a
-- | desktop, and above the list on a phone, whose heading row is the
-- | overlay's. Nothing is drawn under the heading until the list has come,
-- | with the time it came at.
notifications :: ∀ w m. MonadEffect m =>
    { phone :: Boolean
    , loaded :: Maybe { now :: Instant, list :: Array Notification }
    , onOpen :: Notification -> MouseEvent -> m Unit
    , onReadAll :: m Unit
    }
    -> Array (HH.HTML w (m Unit))
notifications { phone, loaded, onOpen, onReadAll } = let
    unread = maybe false (_.list >>> any (not <<< _.read)) loaded
    markAllRead = button Text Small onReadAll [ HH.text "Mark all read" ]
    heading
        | phone = []
        | otherwise =
            [ HH.div [ HS.class_ "header-menu-heading" ] $
                [ HH.h2_ [ HH.text "Notifications" ] ] <> if unread then [ markAllRead ] else []
            ]
    content = case loaded of
        Nothing -> []
        Just { list: [] } ->
            [ HH.div [ HS.class_ "notifications-empty" ]
                [ HH.p_ [ HH.text "No notifications yet. Every post tells you when someone new fits it, and before it expires." ]
                , buttonLink Outline Small "/post" [ Icons.plus, HH.text "New post" ]
                ]
            ]
        Just { now, list: list' } ->
            (if phone && unread then [ HH.div [ HS.class_ "notifications-actions" ] [ markAllRead ] ] else [])
            <>
            [ HH.div [ HS.class_ "notifications" ] $ groups list' <#> \group ->
                HH.section [ HS.class_ "notification-group" ] $
                [ HH.h3 [ HS.class_ "notification-heading" ]
                    [ HH.text $ inboxPostName group.post <> " · " <> group.post.game <> " " <> group.post.type ]
                ]
                <> (group.rows <#> row now onOpen)
            ]
    in
    heading <> content
