module Bcrypt.Async where

import Prelude

import Async (Async)
import Async as Async
import Bcrypt as Bcrypt
import Node.Errors (Error)

genSalt :: Int -> Char -> Async Error String
genSalt rounds minor = Async.fromEitherCont $ Bcrypt.genSalt rounds minor

genSalt_ :: Int -> Async Error String
genSalt_ rounds = Async.fromEitherCont $ Bcrypt.genSalt_ rounds

genSalt__ :: Async Error String
genSalt__ = Async.fromEitherCont Bcrypt.genSalt__

hash :: String -> String -> Async Error String
hash data' salt = Async.fromEitherCont $ Bcrypt.hash data' salt

hash' :: String -> Int -> Async Error String
hash' data' rounds = Async.fromEitherCont $ Bcrypt.hash' data' rounds

hash_ :: String -> Async Error String
hash_ data' = Async.fromEitherCont $ Bcrypt.hash_ data'

compare :: String -> String -> Async Error Boolean
compare data' encrypted = Async.fromEitherCont $ Bcrypt.compare data' encrypted
