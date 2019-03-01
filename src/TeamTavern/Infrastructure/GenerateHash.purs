module TeamTavern.Infrastructure.GenerateHash
    (Hash, GenerateHashError, generateHash, generateHash', unHash) where

import Prelude

import Async (Async)
import Bcrypt.Async (hash_)
import Data.Bifunctor (bimap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Variant (SProxy(..), Variant, inj)
import Node.Errors (Error)

newtype Hash = Hash String

derive instance genericHash :: Generic Hash _

instance showHash :: Show Hash where show = genericShow

type GenerateHashError errors = Variant (bcryptError :: Error | errors)

generateHash :: forall errors. String -> Async (GenerateHashError errors) Hash
generateHash password =
    password # hash_ # bimap (inj (SProxy :: SProxy "bcryptError")) Hash

generateHash' :: forall password errors.
    (password -> String)-> password -> Async (GenerateHashError errors) Hash
generateHash' toString password = generateHash $ toString password

unHash :: Hash -> String
unHash (Hash hash) = hash
