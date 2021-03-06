module TeamTavern.Client.Pages.Player.Status where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import TeamTavern.Client.Script.Cookie (getPlayerNickname)

data Status = SignedInSelf | SignedInOther | SignedOut

getStatus :: forall effect. MonadEffect effect => String -> effect Status
getStatus nickname = do
    nickname' <- getPlayerNickname
    pure $ case nickname' of
        Just nickname'' | nickname'' == nickname -> SignedInSelf
        Just nickname'' -> SignedInOther
        Nothing -> SignedOut
