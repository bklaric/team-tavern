module TeamTavern.Server.Team.Infrastructure.ValidateDiscordServer where

import Prelude

import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import Data.Validated as Validated
import TeamTavern.Server.Infrastructure.Error (ValidatedTerrorNeaVar)
import TeamTavern.Server.Infrastructure.Error as Terror
import TeamTavern.Server.Profile.Infrastructure.ValidateUrl (Url, validateUrlV)
import Type.Proxy (Proxy(..))

validateDiscordServer :: forall errors.
    Maybe String -> ValidatedTerrorNeaVar (discordServer :: {} | errors) (Maybe Url)
validateDiscordServer discordServer
    = discordServer
    # traverse (validateUrlV "discord.gg")
    # Validated.lmap
        ((\errors -> Terror.singleton {} ("Error validating Discord server: " <> show errors))
        >>> Terror.labelNea (Proxy :: _ "discordServer"))
