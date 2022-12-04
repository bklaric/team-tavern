module TeamTavern.Server.Player.Domain.Hash where

import Prelude

import Async (Async)
import Bcrypt.Async (hash_)
import Data.Bifunctor (bimap)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Jarilo (internal__)
import TeamTavern.Server.Infrastructure.Error (InternalError_, TavernError(..))
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Player.Domain.Password (Password(..))

newtype Hash = Hash String

derive instance newtypeHash :: Newtype Hash _

derive instance genericHash :: Generic Hash _

instance showHash :: Show Hash where show = genericShow

generateHash :: forall errors. Password -> Async (InternalError_ errors) Hash
generateHash (Password password) =
    password # hash_ # bimap
        (\error -> TavernError internal__ [ "Error generating password hash: " <> print error ])
        Hash
