module TeamTavern.Server.Game.Domain.Description where

import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.Validated (Validated)
import TeamTavern.Server.Domain.Paragraph (Paragraph)
import TeamTavern.Server.Domain.NonEmptyText (NonEmptyTextError)
import TeamTavern.Server.Domain.NonEmptyText as NonEmptyText

newtype Description = Description (Array Paragraph)

derive instance newtypeDescription :: Newtype Description _

maxLength :: Int
maxLength = 2000

create :: String -> Validated (NonEmptyList NonEmptyTextError) Description
create = NonEmptyText.create maxLength Description