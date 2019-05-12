module TeamTavern.Server.Game.Domain.Handle where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List.Types (NonEmptyList)
import Data.Newtype (class Newtype)
import Data.String (trim)
import Data.Validated (Validated)
import Data.Variant (Variant)
import Jarilo.FromComponent (class FromComponent)
import Wrapped.String (Empty, NotAsciiAlphaNumHyphen, TooLong, empty, notAsciiAlphaNumHyphen, tooLong)
import Wrapped.Validated as Wrapped

newtype Handle = Handle String

derive instance eqHandle :: Eq Handle

derive instance newtypeHandle :: Newtype Handle _

derive instance genericHandle :: Generic Handle _

instance showHandle :: Show Handle where show = genericShow

derive newtype instance fromComponentHandle :: FromComponent Handle

type HandleError = Variant
    ( empty :: Empty
    , tooLong :: TooLong
    , notAsciiAlphaNumHyphen :: NotAsciiAlphaNumHyphen
    )

maxLength :: Int
maxLength = 50

create :: String -> Validated (NonEmptyList HandleError) Handle
create handle =
    Wrapped.create trim [empty, tooLong maxLength, notAsciiAlphaNumHyphen]
    Handle handle
