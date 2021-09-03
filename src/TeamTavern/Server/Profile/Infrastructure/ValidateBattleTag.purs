module TeamTavern.Server.Profile.Infrastructure.ValidateBattleTag (BattleTag, toString, validateBattleTag) where

import Prelude

import Data.Maybe (Maybe)
import Data.String (Pattern(..), length, split)
import Data.Validated.Label (ValidatedVariants)
import Data.Variant (SProxy(..))
import TeamTavern.Server.Profile.Infrastructure.ValidateContact (validateContact)

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

validateBattleTag :: forall errors. Maybe String -> ValidatedVariants (battleTag :: String | errors) (Maybe BattleTag)
validateBattleTag battleTag =
    validateContact battleTag isBattleTagValid BattleTag (SProxy :: _ "battleTag") ("Invalid BattleTag: " <> _)
