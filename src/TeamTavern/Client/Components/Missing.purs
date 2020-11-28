module TeamTavern.Client.Components.Missing where

import Halogen.HTML as HH
import TeamTavern.Client.Snippets.Class as HS

missing :: forall slots action. String -> HH.HTML slots action
missing text = HH.p [ HS.class_ "missing" ] [ HH.text text ]
