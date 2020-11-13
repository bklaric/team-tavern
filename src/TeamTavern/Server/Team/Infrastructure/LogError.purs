module TeamTavern.Server.Team.Infrastructure.LogError where

import Prelude

import Data.Array as Array
import Data.Symbol (SProxy(..))
import Data.Variant (match)
import Effect (Effect, foreachE)
import Prim.Row (class Lacks)
import Record.Builder (Builder)
import Record.Builder as Builder
import TeamTavern.Server.Infrastructure.Log (logLines)
import TeamTavern.Server.Team.Infrastructure.ValidateTeam (TeamErrors)

teamHandler :: forall fields. Lacks "team" fields =>
    Builder (Record fields) { team :: TeamErrors -> Effect Unit | fields }
teamHandler = Builder.insert (SProxy :: SProxy "team") \errors ->
    foreachE (Array.fromFoldable errors) $ match
    { name: logLines, website: logLines, discordServer: logLines, about: logLines }
