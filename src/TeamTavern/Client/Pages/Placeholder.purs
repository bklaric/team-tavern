module TeamTavern.Client.Pages.Placeholder (placeholder) where

import Halogen.HTML as HH
import TeamTavern.Client.Snippets.Class as HS

-- What a page the redesign has not built yet shows in its place.
placeholder :: ∀ slots action. String -> HH.HTML slots action
placeholder name = HH.div [ HS.class_ "placeholder" ] [ HH.h1_ [ HH.text name ] ]
