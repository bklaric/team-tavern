module TeamTavern.Server.Profile.Infrastructure.ValidateRiotId (RiotId, toString, validateRiotId) where

import Prelude

import Data.Either (Either(..))
import Data.String (Pattern(..), length, split, trim)

newtype RiotId = RiotId String

toString :: RiotId -> String
toString (RiotId riotId) = riotId

minNameLength :: Int
minNameLength = 3

maxNameLength :: Int
maxNameLength = 16

minDiscriminatorLength :: Int
minDiscriminatorLength = 3

maxDiscriminatorLength :: Int
maxDiscriminatorLength = 5

isRiotIdValid :: String -> Boolean
isRiotIdValid riotId =
    case split (Pattern "#") riotId of
    [ username, discriminator ] ->
        minNameLength <= length username
        && length username <= maxNameLength
        && minDiscriminatorLength <= length discriminator
        && length discriminator <= maxDiscriminatorLength
    _ -> false

validateRiotId :: String -> Either (Array String) RiotId
validateRiotId riotId | riotId' <- trim riotId =
    if isRiotIdValid riotId'
    then Right $ RiotId riotId'
    else Left [ "Invalid Riot ID: " <> riotId' ]
