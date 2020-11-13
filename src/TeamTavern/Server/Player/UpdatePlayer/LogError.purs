module TeamTavern.Server.Player.UpdatePlayer.LogError where

import Prelude

import Data.Array as Array
import Data.Variant (SProxy(..), Variant, match)
import Effect (Effect, foreachE)
import Prim.Row (class Lacks)
import Record.Builder (Builder)
import Record.Builder as Builder
import TeamTavern.Server.Infrastructure.Log (clientHandler, internalHandler, logLines, notAuthorizedHandler)
import TeamTavern.Server.Infrastructure.Log as Log
import TeamTavern.Server.Player.UpdatePlayer.ValidatePlayer (PlayerErrors)

type UpdateDetailsError = Variant
    ( internal :: Array String
    , client :: Array String
    , notAuthorized :: Array String
    , player :: PlayerErrors
    )

playerHandler :: forall fields. Lacks "player" fields =>
    Builder (Record fields) { player :: PlayerErrors -> Effect Unit | fields }
playerHandler = Builder.insert (SProxy :: SProxy "player") \errors ->
    foreachE (Array.fromFoldable errors) $ match
    { discordTag: logLines, about: logLines }

logError :: UpdateDetailsError -> Effect Unit
logError = Log.logError "Error updating player"
    (internalHandler >>> clientHandler >>> notAuthorizedHandler >>> playerHandler)
