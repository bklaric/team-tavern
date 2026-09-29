module TeamTavern.Routes.Account.ViewAccount where

import Data.Maybe (Maybe)
import Jarilo (type (!), type (==>), Get_, Internal_, Literal, NotAuthorized_, OkJson)

-- | Everything the account page shows (brief 11.5).
type ViewAccount =
    Get_ (Literal "account")
    ==> OkJson OkContent ! NotAuthorized_ ! Internal_

-- | A kind of contact some game offers, with the account the player holds for
-- | it and the games whose posts show it, by title. `everyGame` is every game
-- | in the catalogue offering it.
type Contact =
    { kind :: String
    , value :: Maybe String
    , games :: Array String
    , everyGame :: Boolean
    }

type Switches =
    { matches :: Boolean
    , messages :: Boolean
    , renewals :: Boolean
    }

-- | `signIn` is `password` or `discord`. `conversations` counts those on the
-- | player's posts and those they started, which deleting the account deletes.
type OkContent =
    { nickname :: String
    , birthday :: Maybe String
    , country :: Maybe String
    , languages :: Array String
    , timezone :: Maybe String
    , contacts :: Array Contact
    , email :: Maybe String
    , emailConfirmed :: Boolean
    , signIn :: String
    , switches :: Switches
    , posts :: Int
    , conversations :: Int
    }
