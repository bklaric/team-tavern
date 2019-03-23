module TeamTavern.Player.Domain.About where

import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.Validated (Validated)
import TeamTavern.Domain.Paragraph (Paragraph)
import TeamTavern.Domain.Text (TextError)
import TeamTavern.Domain.Text as Text

newtype About = About (Array Paragraph)

derive instance newtypeAbout :: Newtype About _

maxLength :: Int
maxLength = 2000

create :: String -> Validated (NonEmptyList TextError) About
create = Text.create maxLength About
