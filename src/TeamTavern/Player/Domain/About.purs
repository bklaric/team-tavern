module TeamTavern.Player.Domain.About where

import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.Validated (Validated)
import Data.Variant (Variant)
import Wrapped.String (NotPrintable, TooLong, notPrintable, tooLong)
import Wrapped.Validated as Wrapped

newtype About = About String

derive instance newtypeAbout :: Newtype About _

type AboutError = Variant
    ( tooLong :: TooLong
    , notPrintable :: NotPrintable
    )

maxLength :: Int
maxLength = 2000

create :: String -> Validated (NonEmptyList AboutError) About
create about = Wrapped.create trim [tooLong maxLength, notPrintable] About about
