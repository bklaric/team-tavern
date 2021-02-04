module TeamTavern.Server.Profile.Infrastructure.ValidateBattleTag (BattleTag, toString, validateBattleTag) where

import Prelude

import Data.Either (Either(..))
import Data.String (Pattern(..), length, split, trim)

newtype BattleTag = BattleTag String

toString :: BattleTag -> String
toString (BattleTag battleTag) = battleTag

minNameLength :: Int
minNameLength = 3

maxNameLength :: Int
maxNameLength = 12

minDiscriminatorLength :: Int
minDiscriminatorLength = 4

maxDiscriminatorLength :: Int
maxDiscriminatorLength = 5

isBattleTagValid :: String -> Boolean
isBattleTagValid battleTag =
    case split (Pattern "#") battleTag of
    [ username, discriminator ] ->
        minNameLength <= length username
        && length username <= maxNameLength
        && minDiscriminatorLength <= length discriminator
        && length discriminator <= maxDiscriminatorLength
    _ -> false

validateBattleTag :: String -> Either (Array String) BattleTag
validateBattleTag battleTag | battleTag' <- trim battleTag =
    if isBattleTagValid battleTag'
    then Right $ BattleTag battleTag'
    else Left [ "Invalid Riot ID: " <> battleTag' ]
