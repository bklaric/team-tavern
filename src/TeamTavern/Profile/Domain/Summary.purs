module TeamTavern.Profile.Domain.Summary where

import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.Validated (Validated)
import TeamTavern.Domain.Paragraph (Paragraph)
import TeamTavern.Domain.Text (TextError)
import TeamTavern.Domain.Text as Text

newtype Summary = Summary (Array Paragraph)

derive instance newtypeSummary :: Newtype Summary _

maxLength :: Int
maxLength = 2000

create :: String -> Validated (NonEmptyList TextError) Summary
create = Text.create maxLength Summary
