module TeamTavern.Routes.Post.RenewByNonce where

import Jarilo (type (!), type (==>), Internal_, Literal, NotFound_, OkJson, PostJson_)
import TeamTavern.Routes.Shared.Description (Description)

-- | Renews a post from the link in an email, signed out or as anyone (brief 9).
-- | The answer is where the link lands: the post's game, and the description
-- | the post makes, which the feed opens with. Not found is a nonce no post has.
type RenewByNonce =
    PostJson_ (Literal "renew") RequestContent
    ==> OkJson OkContent ! NotFound_ ! Internal_

type RequestContent = { nonce :: String }

type OkContent =
    { handle :: String
    , id :: Int
    , type :: String
    , description :: Description
    }
