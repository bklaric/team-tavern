module TeamTavern.Player.Register.GenerateHash
    (Hash, GenerateHashError, generateHash, unHash) where

import Prelude

import Async (Async)
import Bcrypt.Async (hash_)
import Data.Bifunctor (bimap)
import Data.Variant (SProxy(..), Variant, inj)
import Node.Errors (Error)
import TeamTavern.Player.Register.ValidateModel (Password, unPassword)

newtype Hash = Hash String

type GenerateHashError errors = Variant (bcryptError :: Error | errors)

generateHash :: forall errors. Password -> Async (GenerateHashError errors) Hash
generateHash password =
    unPassword password
    # hash_
    # bimap (inj (SProxy :: SProxy "bcryptError")) Hash

unHash :: Hash -> String
unHash (Hash hash) = hash
