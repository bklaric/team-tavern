module TeamTavern.Server.Player.Domain.About where

import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.Validated (Validated)
import TeamTavern.Server.Domain.Paragraph (Paragraph)
import TeamTavern.Server.Domain.Text (TextError)
import TeamTavern.Server.Domain.Text as Text

newtype About = About (Array Paragraph)

derive instance newtypeAbout :: Newtype About _

maxLength :: Int
maxLength = 2000

create :: String -> Validated (NonEmptyList TextError) About
create = Text.create maxLength About
