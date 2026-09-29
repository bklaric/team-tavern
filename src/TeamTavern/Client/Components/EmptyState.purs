module TeamTavern.Client.Components.EmptyState (emptyState) where

import Halogen.HTML as HH
import TeamTavern.Client.Snippets.Class as HS

-- | A page with nothing to show yet: what would be here, and the one thing to
-- | do about it.
emptyState :: ∀ w i. { heading :: String, text :: String, action :: HH.HTML w i } -> HH.HTML w i
emptyState { heading, text, action } =
    HH.div [ HS.class_ "empty-state" ] [ HH.h2_ [ HH.text heading ], HH.p_ [ HH.text text ], action ]
