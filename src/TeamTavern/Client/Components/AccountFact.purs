module TeamTavern.Client.Components.AccountFact (accountFact) where

import Halogen.HTML as HH
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button)
import TeamTavern.Client.Snippets.Class as HS

-- A fact the account already holds, shown on a form in place of its control
-- until the player asks to change it.
accountFact :: ∀ w i. String -> i -> HH.HTML w i
accountFact value onChange =
    HH.div [ HS.class_ "account-fact" ]
    [ HH.span [ HS.class_ "account-fact-value" ] [ HH.text value ]
    , HH.span [ HS.class_ "account-fact-source" ] [ HH.text "From your account" ]
    , button Text Small onChange [ HH.text "Change" ]
    ]
