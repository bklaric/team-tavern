module TeamTavern.Client.Pages.Team.Status where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import TeamTavern.Client.Script.Cookie (getPlayerNickname)

data Status = SignedInOwner | SignedInOther | SignedOut

derive instance Eq Status

getStatus :: ∀ effect. MonadEffect effect => String -> effect Status
getStatus owner = do
    nickname <- getPlayerNickname
    pure $ case nickname of
        Just nickname' | nickname' == owner -> SignedInOwner
        Just _ -> SignedInOther
        Nothing -> SignedOut
