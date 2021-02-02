module TeamTavern.Server.Profile.Infrastructure.ValidateFriendCode (FriendCode, toString, validateFriendCode) where

import Prelude

import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (isJust)
import Data.String (Pattern(..), length, split, trim)

newtype FriendCode = FriendCode String

toString :: FriendCode -> String
toString (FriendCode friendCode) = friendCode

minNameLength :: Int
minNameLength = 3

maxNameLength :: Int
maxNameLength = 16

-- Fixed format, e.g.: SW-7417-3522-1808
-- https://en-americas-support.nintendo.com/app/answers/detail/a_id/22438/kw/friend%20code/p/989
isFriendCodeValid :: String -> Boolean
isFriendCodeValid friendCode =
    case split (Pattern "-") friendCode of
    [ "SW", left, mid, right ] ->
        length left == 4 && length mid == 4 && length right == 4
        && (isJust $ fromString left) && (isJust $ fromString mid) && (isJust $ fromString right)
    _ -> false

validateFriendCode :: String -> Either (Array String) FriendCode
validateFriendCode friendCode | friendCode' <- trim friendCode =
    if isFriendCodeValid friendCode'
    then Right $ FriendCode friendCode'
    else Left [ "Invalid friend code: " <> friendCode' ]
