module TeamTavern.Game.Domain.Description where

import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.Validated (Validated)
import TeamTavern.Domain.Paragraph (Paragraph)
import TeamTavern.Domain.Text (TextError)
import TeamTavern.Domain.Text as Text

newtype Description = Description (Array Paragraph)

derive instance newtypeDescription :: Newtype Description _

maxLength :: Int
maxLength = 2000

create :: String -> Validated (NonEmptyList TextError) Description
create = Text.create maxLength Description
