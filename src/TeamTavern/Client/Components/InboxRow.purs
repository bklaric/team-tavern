module TeamTavern.Client.Components.InboxRow (inboxCover, inboxPostName, inboxRow) where

import Prelude

import Data.DateTime.Instant (Instant)
import Data.Maybe (fromMaybe)
import Data.String (joinWith)
import Effect.Class (class MonadEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Unread (unreadDot)
import TeamTavern.Client.Script.Ago (ago)
import TeamTavern.Client.Script.Navigate (navigateWithEvent_)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Conversation.ViewInbox (InboxPost, InboxRow)

-- | What the inbox calls a post: a player post by its owner, a group or a
-- | community by its name.
inboxPostName :: InboxPost -> String
inboxPostName post =
    if post.type == "player" then post.owner else fromMaybe (post.owner <> "'s " <> post.type) post.name

inboxCover :: ∀ w i. String -> HH.HTML w i
inboxCover handle = HH.img [ HS.class_ "inbox-cover", HP.src $ "/images/games/" <> handle <> ".webp", HP.alt "" ]

-- | A conversation in the inbox, opening it. Under one of the player's own
-- | posts it is titled with the other player and needs only "You:" before its
-- | last message; about someone else's post it is titled with the post, has its
-- | cover in front, and names whoever wrote last.
inboxRow :: ∀ w m. MonadEffect m =>
    { now :: Instant, current :: Boolean, ownPost :: Boolean, row :: InboxRow } -> HH.HTML w (m Unit)
inboxRow { now, current, ownPost, row } = let
    path = "/messages/" <> show row.id
    sender
        | row.last.mine = "You: "
        | ownPost = ""
        | otherwise = row.last.sender <> ": "
    in
    HH.a
        ( [ HS.class_ if row.unread then "inbox-row inbox-row-unread" else "inbox-row"
          , HP.href path
          , HE.onClick $ navigateWithEvent_ path
          ]
          <> if current then [ HP.attr (HH.AttrName "aria-current") "page" ] else []
        )
    $ (if ownPost then [] else [ inboxCover row.post.handle ])
    <>
    [ HH.span [ HS.class_ "inbox-row-main" ]
        [ HH.span [ HS.class_ "inbox-row-top" ]
            [ HH.span [ HS.class_ "inbox-row-title" ] $
                if ownPost then [ HH.text row.other ]
                else
                    [ HH.text $ inboxPostName row.post <> " "
                    , HH.span [ HS.class_ "inbox-kind" ] [ HH.text $ "· " <> row.post.game <> " " <> row.post.type ]
                    ]
            , HH.span [ HS.class_ "inbox-row-time" ] [ HH.text $ ago now row.last.created ]
            ]
        , HH.span [ HS.class_ "inbox-row-snippet" ] [ HH.text $ sender <> joinWith " " row.last.content ]
        ]
    ]
    <> if row.unread then unreadDot else []
