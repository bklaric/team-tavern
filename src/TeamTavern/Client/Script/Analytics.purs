module TeamTavern.Client.Script.Analytics (track, track_, alias, identify, aliasNickname, identifyNickname, register, registerSignedIn) where

import Prelude

import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import TeamTavern.Client.Script.Cookie (getPlayerNickname, hasPlayerIdCookie)

foreign import trackImpl :: forall props. String -> Record props -> Effect Unit

foreign import trackImpl_ :: String -> Effect Unit

foreign import aliasImpl :: String -> Effect Unit

foreign import identifyImpl :: String -> Effect Unit

foreign import registerImpl :: forall props. Record props -> Boolean -> Effect Unit

-- track :: forall props. String -> Record props -> Effect Unit
track :: forall m props. MonadEffect m => String -> Record props -> m Unit
track event properties = liftEffect $ trackImpl event properties

-- track_ :: String -> Effect Unit
track_ :: forall m. MonadEffect m => String -> m Unit
track_ event = liftEffect $ trackImpl_ event

alias :: forall m. MonadEffect m => String -> m Unit
alias id = liftEffect $ aliasImpl id

identify :: forall m. MonadEffect m => String -> m Unit
identify id = liftEffect $ identifyImpl id

aliasNickname :: forall m. MonadEffect m => m Unit
aliasNickname = do
    registerSignedIn
    getPlayerNickname >>= maybe (pure unit) alias

identifyNickname :: forall m. MonadEffect m => m Unit
identifyNickname = getPlayerNickname >>= maybe (pure unit) identify

register :: forall m props. MonadEffect m => Record props -> Boolean -> m Unit
register properties persistent = liftEffect $ registerImpl properties persistent

registerSignedIn :: forall m. MonadEffect m => m Unit
registerSignedIn = hasPlayerIdCookie <#> { signedIn: _ } >>= flip register false
