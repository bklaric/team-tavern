module TeamTavern.Client.Script.Previous (previousOf, stampPrevious) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Foreign (Foreign)
import Yoga.JSON (read_)

-- | Writes the path of the page the site came from into the current history
-- | entry, beside what the page wrote there, so the entry knows what the
-- | browser's Back returns to after a reload or a trip back and forth.
foreign import stampPrevious :: String -> Effect Unit

-- | The path an entry's state was stamped with, if any.
previousOf :: Foreign -> Maybe String
previousOf state = read_ state >>= \({ previous } :: { previous :: Maybe String }) -> previous
