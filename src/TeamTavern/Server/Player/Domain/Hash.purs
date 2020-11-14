module TeamTavern.Server.Player.Domain.Hash where

import Prelude

import Async (Async)
import Bcrypt.Async (hash_)
import Data.Bifunctor (bimap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Data.Variant (SProxy(..), Variant, inj)
import Node.Errors (Error)
import TeamTavern.Server.Player.Domain.Password (Password(..))

newtype Hash = Hash String

derive instance newtypeHash :: Newtype Hash _

derive instance genericHash :: Generic Hash _

instance showHash :: Show Hash where show = genericShow

type GenerateHashError errors = Variant (bcrypt :: Error | errors)

generateHash :: forall errors. Password -> Async (GenerateHashError errors) Hash
generateHash (Password password) =
    password # hash_ # bimap (inj (SProxy :: SProxy "bcrypt")) Hash
