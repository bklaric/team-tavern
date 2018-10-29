module TeamTavern.Player.Domain.CharCount
    ( CharCount
    , create
    , ByteCount
    , toByteCount
    ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

newtype CharCount = CharCount Int

derive instance newtypeCharCount :: Newtype CharCount _

create :: Int -> Maybe CharCount
create charCount =
    if (charCount > 0) && (charCount `mod` 2 == 0)
    then Just $ CharCount charCount
    else Nothing

newtype ByteCount = ByteCount Int

derive instance newtypeByteCount :: Newtype ByteCount _

toByteCount :: CharCount -> ByteCount
toByteCount (CharCount charCount) = ByteCount (charCount / 2)
