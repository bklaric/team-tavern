module TeamTavern.Domain.Id (Id, create) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

newtype Id = Id Int

derive instance newtypeId :: Newtype Id _

derive newtype instance showId :: Show Id

create :: Int -> Maybe Id
create id =
    if id > 0
    then Just $ Id id
    else Nothing
