module TeamTavern.Server.Profile.Domain.Summary where

import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.Validated (Validated)
import TeamTavern.Server.Domain.Paragraph (Paragraph)
import TeamTavern.Server.Domain.NonEmptyText (NonEmptyTextError)
import TeamTavern.Server.Domain.NonEmptyText as NonEmptyText

newtype Summary = Summary (Array Paragraph)

derive instance newtypeSummary :: Newtype Summary _

maxLength :: Int
maxLength = 2000

create :: String -> Validated (NonEmptyList NonEmptyTextError) Summary
create = NonEmptyText.create maxLength Summary
