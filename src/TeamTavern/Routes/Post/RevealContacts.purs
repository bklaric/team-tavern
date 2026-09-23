module TeamTavern.Routes.Post.RevealContacts where

import Data.Maybe (Maybe)
import Jarilo (type (!), type (/), type (==>), Capture, Internal_, Literal, NoBody, NotAuthorized_, NotFound_, OkJson, Post_)

-- | The contacts and join links a post's owner shared, for its contact panel,
-- | which counts the reveal on the post (brief 5.6). Not found is a post that
-- | isn't the game's, or one either side has blocked the other from.
type RevealContacts =
    Post_ (Literal "games" / Capture "handle" String / Literal "posts" / Capture "id" Int / Literal "contacts") NoBody
    ==> OkJson OkContent ! NotAuthorized_ ! NotFound_ ! Internal_

-- | `contacts` are the owner's accounts of the game's contact kinds, Discord
-- | first; a community has none, only its links.
type OkContent =
    { contacts :: Array { kind :: String, value :: String }
    , discord_server :: Maybe String
    , website :: Maybe String
    }
