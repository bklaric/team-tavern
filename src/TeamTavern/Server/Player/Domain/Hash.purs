module TeamTavern.Server.Player.Domain.Hash where

import Prelude

import Async (Async)
import Bcrypt.Async (hash_)
import Data.Bifunctor (bimap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.Variant (SProxy(..), inj)
import TeamTavern.Server.Infrastructure.Error (InternalError)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Player.Domain.Password (Password(..))

newtype Hash = Hash String

derive instance newtypeHash :: Newtype Hash _

derive instance genericHash :: Generic Hash _

instance showHash :: Show Hash where show = genericShow

generateHash :: forall errors. Password -> Async (InternalError errors) Hash
generateHash (Password password) =
    password # hash_ # bimap
        ( \error -> inj (SProxy :: SProxy "internal")
            [ "Error generating password hash: " <> print error ]
        )
        Hash
