module TeamTavern.Server.Profile.Infrastructure.ValidateRiotId (RiotId, toString, validateRiotId) where

import Prelude

import Data.Maybe (Maybe)
import Data.String (Pattern(..), length, split)
import TeamTavern.Server.Infrastructure.Error (ValidatedTerrorNeaVar)
import TeamTavern.Server.Profile.Infrastructure.ValidateContact (validateContact)
import Type.Proxy (Proxy(..))

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

validateRiotId :: forall errors.
    Maybe String -> ValidatedTerrorNeaVar (riotId :: {} | errors) (Maybe RiotId)
validateRiotId riotId =
    validateContact riotId isRiotIdValid RiotId (Proxy :: _ "riotId") ("Invalid RiotId: " <> _)
