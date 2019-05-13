module TeamTavern.Server.Game.Domain.Title where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.Validated (Validated)
import Data.Variant (Variant)
import Wrapped.String (Empty, NotPrintable, TooLong, empty, notPrintable, tooLong)
import Wrapped.Validated as Wrapped

newtype Title = Title String

derive instance eqTitle :: Eq Title

derive instance newtypeTitle :: Newtype Title _

derive instance genericTitle :: Generic Title _

instance showTitle :: Show Title where
    show = genericShow

type TitleError = Variant
    ( empty :: Empty
    , tooLong :: TooLong
    , notPrintable :: NotPrintable
    )

maxLength :: Int
maxLength = 50

create :: String -> Validated (NonEmptyList TitleError) Title
create title =
    Wrapped.create trim [empty, tooLong maxLength, notPrintable] Title title