module TeamTavern.Client.Components.OwnPostStatus (ownPostStatus, renewDue) where

import Prelude

import Data.Array (catMaybes)
import Data.DateTime.Instant (Instant, unInstant)
import Data.Int (round)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Script.Ago (ago, millisOf)
import TeamTavern.Client.Snippets.Class as HS
import Web.UIEvent.MouseEvent (MouseEvent)

day :: Number
day = 24.0 * 60.0 * 60.0 * 1000.0

plural :: Int -> String -> String
plural count noun = show count <> " " <> noun <> if count == 1 then "" else "s"

data Term = Active Int | Expiring Int | Expired

-- The last week is when the renewal email goes out (brief 9).
termOf :: Instant -> String -> Term
termOf now expires = let
    left = millisOf expires - unwrap (unInstant now)
    days = round $ left / day
    in
    if left <= 0.0 then Expired else if days <= 7 then Expiring days else Active days

-- | Whether the post is in its last week or past it, when Renew is what its
-- | owner should think of doing.
renewDue :: Instant -> String -> Boolean
renewDue now expires = case termOf now expires of
    Active _ -> false
    _ -> true

-- | What a post's owner is told about it under its facts (brief 11.2): how long
-- | it stays active, or what expiry means now that it has, how many
-- | conversations it produced, how many of them are unread, and how often its
-- | contacts were shown. The conversations open in the inbox (brief 11.2) where
-- | there are any, on `conversation`, through `onOpen`.
ownPostStatus :: ∀ w i.
    { now :: Instant
    , expires :: String
    , conversations :: Int
    , unread :: Int
    , conversation :: Maybe Int
    , reveals :: Int
    , onOpen :: String -> MouseEvent -> i
    }
    -> HH.HTML w i
ownPostStatus { now, expires, conversations, unread, conversation, reveals, onOpen } = let
    state = case termOf now expires of
        Expired ->
            HH.span [ HS.class_ "own-post-state" ]
            [ Icons.clock
            , HH.text $ "Expired " <> ago now expires
                <> ". It's listed under older posts, and match emails are paused."
            ]
        -- The last week says so in full, with its icon, so it doesn't rely on color.
        Expiring days ->
            HH.span [ HS.class_ "own-post-state own-post-state-soon" ]
            [ Icons.circleAlert
            , HH.text if days == 0 then "Expires today" else "Expires in " <> plural days "day"
            ]
        Active days ->
            HH.span [ HS.class_ "own-post-state" ]
            [ Icons.clock, HH.text $ "Active for " <> show days <> " more days" ]
    stat icon text = HH.span [ HS.class_ "own-post-stat" ] [ icon, HH.text text ]
    conversationsStat = case conversation of
        Just id | conversations > 0 -> let path = "/messages/" <> show id in
            HH.a [ HS.class_ "own-post-stat", HP.href path, HE.onClick $ onOpen path ]
        _ -> HH.span [ HS.class_ "own-post-stat" ]
    in
    HH.div [ HS.class_ "own-post-status" ] $ catMaybes
    [ Just state
    , Just $ conversationsStat $
        [ Icons.messageCircle
        , HH.text if conversations == 0 then "No conversations yet" else plural conversations "conversation"
        ]
        <> if unread == 0 then []
            else [ HH.span [ HS.class_ "own-post-unread" ] [ HH.text $ show unread <> " unread" ] ]
    , if reveals == 0 then Nothing
        else Just $ stat Icons.eye $ "Contacts shown " <> plural reveals "time"
    ]
