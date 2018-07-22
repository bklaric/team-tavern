module TeamTavern.Domain.Id where

import Prelude

import Data.Maybe (Maybe(..))

newtype Id = Id Int

derive newtype instance showId :: Show Id

create :: Int -> Maybe Id
create id =
    if id > 0
    then Just $ Id id
    else Nothing
