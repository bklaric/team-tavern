module TeamTavern.Client.Snippets.Class where

import Prelude

import Halogen as HH
import Halogen.HTML.Properties as HP

class_ :: ∀ r i. String -> HP.IProp (class :: String | r) i
class_ = HP.class_ <<< HH.ClassName
