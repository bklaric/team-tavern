module TeamTavern.Server.Player.Domain.Hash where

import Prelude

import Async (Async)
import Bcrypt.Async (hash_)
import Data.Bifunctor (bimap)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Jarilo (internal__)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)
import TeamTavern.Server.Player.Domain.Password (Password(..))

newtype Hash = Hash String

derive instance Newtype Hash _

derive instance Generic Hash _

instance Show Hash where show = genericShow

generateHash :: forall errors. Password -> Async (InternalTerror_ errors) Hash
generateHash (Password password) =
    password # hash_ # bimap
        (\error -> Terror internal__ [ "Error generating password hash: " <> print error ])
        Hash
