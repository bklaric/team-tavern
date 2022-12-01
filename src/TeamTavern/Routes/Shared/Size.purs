module TeamTavern.Routes.Shared.Size where

import Prelude

import Control.Monad.Except (except)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Foreign (ForeignError(..), readString)
import Jarilo.Shared.Component (class Component)
import Yoga.JSON (class ReadForeign, class WriteForeign, writeImpl)

data Size = Party | Community

derive instance eqSize :: Eq Size

fromString :: String -> Maybe Size
fromString "party" = Just Party
fromString "community" = Just Community
fromString _ = Nothing

fromString' :: String -> Either String Size
fromString' size = fromString size # note ("Unknown size: " <> size)

toString :: Size -> String
toString Party = "party"
toString Community = "community"

instance readForeignSize :: ReadForeign Size where
    readImpl size' =
        readString size'
        >>= (fromString' >>> lmap (ForeignError >>> NonEmptyList.singleton) >>> except)

instance writeForeignSize :: WriteForeign Size where
    writeImpl size = writeImpl $ toString size

instance fromComponentSize :: Component Size where
    fromComponent = fromString'
    toComponent = toString
