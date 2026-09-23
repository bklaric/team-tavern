module TeamTavern.Client.Components.Thread (newFrom, olderPostNote, thread) where

import Prelude

import Data.Array (findIndex, mapWithIndex, (!!))
import Data.DateTime.Instant (Instant)
import Data.Maybe (Maybe(..), maybe)
import Data.String (joinWith)
import Halogen.HTML as HH
import Halogen.HTML.Properties.ARIA as HPA
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Script.Ago (millisOf)
import TeamTavern.Client.Script.Day (dayLabel, timeOfDay)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.Card (CardRow)
import TeamTavern.Routes.Shared.Conversation (Message)

-- | Where the New line goes: before the first message the other side wrote
-- | after the viewer had read to (brief 10, Inbox), if there is one.
newFrom :: Maybe String -> Array Message -> Maybe Int
newFrom readTo = findIndex \message ->
    not message.mine && maybe true (\read -> millisOf message.created > millisOf read) readTo

-- A run of messages from one side, close together, shares one line of who and
-- when. The New line and a new day end a run too.
runGap :: Number
runGap = 15.0 * 60.0 * 1000.0

-- | A conversation's messages: the other side's on the left, the viewer's on
-- | the right, under a line for each day.
thread :: ∀ w i.
    { now :: Instant, other :: String, messages :: Array Message, newFrom :: Maybe Int } -> HH.HTML w i
thread { now, other, messages, newFrom: new } =
    HH.div [ HS.class_ "thread" ] $ join $ messages # mapWithIndex \index message -> let
        day = dayLabel now message.created
        who = if message.mine then "You" else other
        newDay = maybe true (\previous -> dayLabel now previous.created /= day) (messages !! (index - 1))
        runEnds = case messages !! (index + 1) of
            Nothing -> true
            Just next ->
                next.mine /= message.mine
                || new == Just (index + 1)
                || millisOf next.created - millisOf message.created > runGap
                || dayLabel now next.created /= day
        in
        (if newDay then [ HH.div [ HS.class_ "thread-day" ] [ HH.text day ] ] else [])
        <> (if new == Just index then [ HH.div [ HS.class_ "thread-new" ] [ HH.text "New" ] ] else [])
        <>
        [ HH.div [ HS.class_ if message.mine then "message message-own" else "message" ]
            [ HH.span [ HS.class_ "visually-hidden" ] [ HH.text $ who <> ": " ]
            , HH.text $ joinWith "\n" message.content
            ]
        ]
        <> if runEnds
            then
                [ HH.div
                    [ HS.class_ if message.mine then "message-meta message-meta-own" else "message-meta"
                    , HPA.hidden "true"
                    ]
                    [ HH.text $ who <> " · " <> timeOfDay message.created ]
                ]
            else []

-- | What a conversation about an expired post says above its thread, to the
-- | side that isn't the owner (brief 9).
olderPostNote :: ∀ w i. CardRow -> Maybe (HH.HTML w i)
olderPostNote post
    | post.expired = Just $ HH.span [ HS.class_ "field-note" ]
        [ Icons.info, HH.text $ "This is an older post. " <> post.owner <> " may no longer be looking." ]
    | otherwise = Nothing
