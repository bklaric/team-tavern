module TeamTavern.Server.Session.Domain.Token (Token(..), TokenHash, generate, hash, sessionDays) where

import Prelude

import Async (Async, fromEffect, fromEither)
import Data.Bifunctor (lmap)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Jarilo (internal__)
import JavaScript.Node.Crypto (hashHex)
import TeamTavern.Server.Infrastructure.Error (Terror(..))
import TeamTavern.Server.Infrastructure.GenerateHexString (ByteCount(..), generateHexString)
import TeamTavern.Server.Infrastructure.Log (print)
import TeamTavern.Server.Infrastructure.Response (InternalTerror_)

newtype Token = Token String

derive instance Newtype Token _

derive instance Generic Token _

instance Show Token where show = genericShow

-- | What the database keeps of a token, so that the session table names no
-- | session a browser could hold. The token is random, so a plain SHA-256
-- | keeps it as hidden as a slow hash would.
newtype TokenHash = TokenHash String

-- | How long a session lasts from its last use, and its cookie from the last
-- | page that renewed it.
sessionDays :: Int
sessionDays = 365

tokenByteCount :: ByteCount
tokenByteCount = ByteCount 20

generate :: ∀ errors. Async (InternalTerror_ errors) Token
generate = generateHexString tokenByteCount <#> Token

hash :: ∀ errors. Token -> Async (InternalTerror_ errors) TokenHash
hash token = do
    result <- hashHex "sha256" (unwrap token) # fromEffect
    result # lmap (\error -> Terror internal__ [ "Hashing a session token failed: " <> print error ])
        # fromEither <#> TokenHash
