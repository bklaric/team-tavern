module TeamTavern.Server.Player.Update.ValidateLangugase where

import Prelude

import Data.Array as Array
import TeamTavern.Server.Infrastructure.Languages (allLanguages)

newtype Language = Language String

validateLanguages :: Array String -> Array Language
validateLanguages languages =
    languages
    # Array.filter (\language -> allLanguages # Array.any (_ == language))
    <#> Language
