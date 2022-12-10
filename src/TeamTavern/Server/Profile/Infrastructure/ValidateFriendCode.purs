module TeamTavern.Server.Profile.Infrastructure.ValidateFriendCode (FriendCode, toString, validateFriendCode) where

import Prelude

import Data.Int (fromString)
import Data.Maybe (Maybe, isJust)
import Data.String (Pattern(..), length, split)
import TeamTavern.Server.Infrastructure.Error (ValidatedTerrorNeaVar)
import TeamTavern.Server.Profile.Infrastructure.ValidateContact (validateContact)
import Type.Proxy (Proxy(..))

newtype FriendCode = FriendCode String

toString :: FriendCode -> String
toString (FriendCode friendCode) = friendCode

-- Fixed format, e.g.: SW-7417-3522-1808
-- https://en-americas-support.nintendo.com/app/answers/detail/a_id/22438/kw/friend%20code/p/989
isFriendCodeValid :: String -> Boolean
isFriendCodeValid friendCode =
    case split (Pattern "-") friendCode of
    [ "SW", left, mid, right ] ->
        length left == 4 && length mid == 4 && length right == 4
        && (isJust $ fromString left) && (isJust $ fromString mid) && (isJust $ fromString right)
    _ -> false

validateFriendCode :: ∀ errors.
    Maybe String -> ValidatedTerrorNeaVar (friendCode :: {} | errors) (Maybe FriendCode)
validateFriendCode friendCode =
    validateContact friendCode isFriendCodeValid FriendCode (Proxy :: _ "friendCode") ("Invalid FriendCode: " <> _)
