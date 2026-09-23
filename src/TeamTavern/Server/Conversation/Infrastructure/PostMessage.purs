module TeamTavern.Server.Conversation.Infrastructure.PostMessage (validateMessage, postMessage) where

import Prelude

import Async (Async, left, note)
import Data.Array (dropWhile, null, reverse)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split, trim)
import Data.String.CodeUnits as CodeUnits
import Data.String.Regex (Regex, replace)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Jarilo (BadRequestRow_, badRequest__, internal__)
import JavaScript.Npm.Pg.Client (Client)
import JavaScript.Npm.Pg.Query (Query(..), (:), (:|))
import TeamTavern.Routes.Shared.Conversation (Conversation)
import TeamTavern.Server.Conversation.Infrastructure.LoadConversation (loadConversation, markRead)
import TeamTavern.Server.Conversation.Infrastructure.SendMessageEmail (MessageEmail)
import TeamTavern.Server.Conversation.Infrastructure.Unread (unreadFor)
import TeamTavern.Server.Infrastructure.Error (Terror(..), TerrorVar, elaborate)
import TeamTavern.Server.Infrastructure.Postgres (LoadSingleError, queryFirstNotFound, queryNone)
import Type.Row (type (+))

trailingSpace :: Regex
trailingSpace = unsafeRegex """\s+$""" noFlags

-- | The longest message, counted as a textarea's `maxlength` counts.
maxLength :: Int
maxLength = 2000

-- | A message as it is kept: its lines, without the blank ones it starts or
-- | ends with or the spaces each ends with. Nothing is sent where nothing is
-- | written.
validateMessage :: ∀ errors. String -> Async (TerrorVar (BadRequestRow_ + errors)) (Array String)
validateMessage text = let
    blank line = trim line == ""
    lines = split (Pattern "\n") text
        <#> replace trailingSpace ""
        # dropWhile blank # reverse # dropWhile blank # reverse
    in
    if null lines || CodeUnits.length (joinWith "\n" lines) > maxLength
    then left $ Terror badRequest__ [ "Message is empty or longer than " <> show maxLength <> " characters." ]
    else pure lines

-- Locks the conversation, so two messages sent at once can't both find it
-- read and both email. Only a side finds it, and not after a block either way.
recipientQuery :: Query
recipientQuery = Query $ """
    select
        """ <> unreadFor "recipient.id" <> """ as had_unread,
        recipient.email_confirmed and recipient.email_messages as wants_email,
        recipient.email,
        recipient.nickname as recipient,
        sender.nickname as sender,
        post.player_id = recipient.id as to_owner,
        game.title as game,
        post.ilk as type,
        post.name,
        owner.nickname as owner,
        post.updated <= now() - case when post.ilk = 'community'
            then interval '90 days' else interval '30 days' end as expired,
        post.renewal_nonce
    from conversation
    join post on post.id = conversation.post_id
    join game on game.id = post.game_id
    join player owner on owner.id = post.player_id
    join player sender on sender.id = $2
    join player recipient on recipient.id =
        case when post.player_id = $2 then conversation.messager_id else post.player_id end
    where conversation.id = $1
        and $2 in (post.player_id, conversation.messager_id)
        and not exists (
            select from block
            where blocker_id = post.player_id and blocked_id = conversation.messager_id
                or blocker_id = conversation.messager_id and blocked_id = post.player_id)
    for update of conversation
    """

type Recipient =
    { had_unread :: Boolean
    , wants_email :: Boolean
    , email :: Maybe String
    , recipient :: String
    , sender :: String
    , to_owner :: Boolean
    , game :: String
    , type :: String
    , name :: Maybe String
    , owner :: String
    , expired :: Boolean
    , renewal_nonce :: String
    }

insertQuery :: Query
insertQuery = Query """
    insert into message (conversation_id, sender_id, content)
    values ($1, $2, $3::text[])
    """

-- The email goes when the recipient had nothing unread in the conversation
-- (brief 10), and only to a confirmed address whose owner wants it. An owner
-- whose post has expired gets its renewal link too (brief 9).
emailOf :: Int -> Array String -> Recipient -> Maybe MessageEmail
emailOf conversation content recipient = case recipient.email of
    Just to | not recipient.had_unread && recipient.wants_email -> Just
        { to
        , recipient: recipient.recipient
        , sender: recipient.sender
        , about: case recipient.to_owner, recipient.name of
            true, Just name -> "your " <> recipient.game <> " " <> recipient.type <> " " <> name
            true, Nothing -> "your " <> recipient.game <> " post"
            false, Just name -> name
            false, Nothing -> recipient.owner <> "'s " <> recipient.game <> " post"
        , conversation
        , content
        , renewal: if recipient.to_owner && recipient.expired then Just recipient.renewal_nonce else Nothing
        }
    _ -> Nothing

-- | Adds the message to the conversation from `sender`, who has then read it
-- | all, and answers with the conversation as they now read it and the email
-- | the other side is owed, if any. The message's time and the sender's read
-- | mark are both the transaction's, so their own message never reads unread.
postMessage :: ∀ errors.
    Client -> Int -> Int -> Array String
    -> Async (LoadSingleError errors) { conversation :: Conversation, email :: Maybe MessageEmail }
postMessage client id sender content = do
    recipient :: Recipient <- queryFirstNotFound client recipientQuery (id :| sender)
        # lmap (elaborate ("Can't find conversation " <> show id <> " for player " <> show sender))
    queryNone client insertQuery (id : sender :| content)
    markRead client id sender
    conversation <- loadConversation client id sender
        >>= note (Terror internal__ [ "Conversation " <> show id <> " went missing after a message." ])
    pure { conversation, email: emailOf id content recipient }
