module TeamTavern.Client.Pages.Team.Status where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import TeamTavern.Client.Script.Cookie (getPlayerNickname)

data Status = SignedInOwner | SignedInOther | SignedOut

getStatus :: String -> Effect Status
getStatus owner = do
    nickname <- getPlayerNickname
    pure $ case nickname of
        Just nickname' | nickname' == owner -> SignedInOwner
        Just nickname' -> SignedInOther
        Nothing -> SignedOut
