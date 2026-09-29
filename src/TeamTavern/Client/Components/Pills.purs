module TeamTavern.Client.Components.Pills (pills) where

import Prelude

import Data.Array (elem)
import Data.Maybe (Maybe, maybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import TeamTavern.Client.Components.Button (Size(..), Weight(..), button)
import TeamTavern.Client.Components.Field (labelId)
import TeamTavern.Client.Components.Input (Option)
import TeamTavern.Client.Icons as Icons
import TeamTavern.Client.Snippets.Class as HS

-- A field's few options as toggles, several at once or one. `all` offers a
-- button choosing every option, as a slotted field's "Any role" does.
pills :: ∀ w i.
    { id :: String
    , multiple :: Boolean
    , options :: Array Option
    , chosen :: Array String
    , onToggle :: String -> i
    , all :: Maybe { label :: String, onAll :: i }
    }
    -> HH.HTML w i
pills { id, multiple, options, chosen, onToggle, all } =
    HH.div
    [ HS.class_ "pills"
    , HPA.role if multiple then "group" else "radiogroup"
    , HPA.labelledBy $ labelId id
    ]
    $ (options <#> pill)
    <> maybe [] (\{ label, onAll } -> [ button Text Small onAll [ HH.text label ] ]) all
    where
    pill { value, label } =
        HH.label [ HS.class_ "pill" ]
        [ HH.input
            [ HP.type_ if multiple then HP.InputCheckbox else HP.InputRadio
            , HP.name id
            , HP.value value
            , HP.checked $ elem value chosen
            , HE.onChange $ const $ onToggle value
            ]
        , Icons.check
        , HH.text label
        ]
