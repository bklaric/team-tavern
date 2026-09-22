module TeamTavern.Client.Components.Stepper (countRow, stepper) where

import Prelude

import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Snippets.Class as HS

-- A count with Fewer and More, each disabled at its end. onStep gets the new
-- count.
stepper :: ∀ w i. { label :: String, value :: Int, min :: Int, max :: Int, onStep :: Int -> i } -> HH.HTML w i
stepper { label, value, min, max, onStep } =
    HH.div [ HS.class_ "stepper", HPA.role "group", HPA.label label ]
    [ step "Fewer" (value <= min) (value - 1) Icons.minus
    , HH.output [ HPA.live "polite" ] [ HH.text $ show value ]
    , step "More" (value >= max) (value + 1) Icons.plus
    ]
    where
    step stepLabel disabled next icon =
        HH.button
        [ HP.type_ HP.ButtonButton
        , HPA.label stepLabel
        , HP.disabled disabled
        , HE.onClick $ const $ onStep next
        ]
        [ icon ]

-- Steppers and the words between them in one row, "3 players, want 2 to 2
-- more".
countRow :: ∀ w i. Array (HH.HTML w i) -> HH.HTML w i
countRow = HH.div [ HS.class_ "count-row" ]
