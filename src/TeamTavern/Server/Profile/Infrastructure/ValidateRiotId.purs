module TeamTavern.Server.Profile.Infrastructure.ValidateRiotId (RiotId, toString, validateRiotId, validateRiotId') where

import Prelude

import Data.Either (Either(..))
import Data.List.NonEmpty as NEL
import Data.List.NonEmpty as Nel
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, split, trim)
import Data.Validated (invalid, valid)
import Data.Validated.Label (VariantValidated)
import Data.Variant (SProxy(..), inj)

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

validateRiotId' :: forall errors.
    Maybe String -> Boolean -> VariantValidated (riotId :: Array String | errors) (Maybe RiotId)
validateRiotId' Nothing false = valid Nothing
validateRiotId' Nothing true = invalid $ Nel.singleton $ inj (SProxy :: SProxy "riotId")
    [ "No Riot ID provided, but it is required." ]
validateRiotId' (Just riotId) _ | riotId' <- trim riotId =
    if isRiotIdValid riotId'
    then valid $ Just $ RiotId riotId'
    else invalid $ NEL.singleton $ inj (SProxy :: SProxy "riotId")
        [ "Invalid Steam profile URL: " <> riotId' ]
