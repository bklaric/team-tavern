module TeamTavern.Client.Snippets.ArticledNoun (indefiniteNoun) where

import Prelude

import Data.Foldable (any)
import Data.String.Utils (startsWith)

vowels :: Array String
vowels = [ "a", "o", "e", "i", "u", "A", "O", "E", "I", "U" ]

indefiniteNoun :: String -> String
indefiniteNoun noun =
    if vowels # any ((flip startsWith) noun)
    then "an " <> noun
    else "a " <> noun
