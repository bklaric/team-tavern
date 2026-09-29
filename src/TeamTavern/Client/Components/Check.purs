module TeamTavern.Client.Components.Check
    ( check
    , choiceList
    , choices
    , switch
    , switches
    ) where

import Prelude

import Data.Maybe (Maybe, maybe)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import TeamTavern.Client.Components.Field (labelId)
import TeamTavern.Client.Components.Input (Option)
import TeamTavern.Client.Snippets.Class as HS

-- A single checkbox with its sentence, which waits for the form it sits in.
check :: ∀ w i. { id :: String, text :: String, checked :: Boolean, onChange :: Boolean -> i } -> HH.HTML w i
check { id, text, checked, onChange } =
    HH.label [ HS.class_ "check" ]
    [ HH.input [ HP.type_ HP.InputCheckbox, HP.id id, HP.checked checked, HE.onChecked onChange ]
    , HH.text text
    ]

radio :: ∀ w i. String -> String -> String -> (String -> i) -> Array (HH.HTML w i) -> HH.HTML w i
radio name chosen value onChoose content =
    HH.label [ HS.class_ "choice" ] $
    [ HH.input
        [ HP.type_ HP.InputRadio
        , HP.name name
        , HP.value value
        , HP.checked $ value == chosen
        , HE.onChange $ const $ onChoose value
        ]
    ]
    <> content

-- A vertical radio group, for a question such as how people reach you.
choiceList :: ∀ w i.
    { id :: String, options :: Array Option, chosen :: String, onChoose :: String -> i } -> HH.HTML w i
choiceList { id, options, chosen, onChoose } =
    HH.div [ HS.class_ "choice-list", HPA.role "radiogroup", HPA.labelledBy $ labelId id ] $
    options <#> \{ value, label } -> radio id chosen value onChoose [ HH.text label ]

-- Radio cards side by side, each with its icon, one column on a phone: the
-- three post types in the player's own words.
choices :: ∀ w i.
    { name :: String
    , label :: String
    , options :: Array { value :: String, label :: String, icon :: HH.HTML w i }
    , chosen :: String
    , onChoose :: String -> i
    }
    -> HH.HTML w i
choices { name, label, options, chosen, onChoose } =
    HH.div [ HS.class_ "choices", HPA.role "radiogroup", HPA.label label ] $
    options <#> \option -> radio name chosen option.value onChoose [ option.icon, HH.text option.label ]

-- On or off the moment it is flipped, where a checkbox waits for its form.
switch :: ∀ w i.
    { id :: String, text :: String, note :: Maybe String, checked :: Boolean, onChange :: Boolean -> i }
    -> HH.HTML w i
switch { id, text, note, checked, onChange } =
    HH.label [ HS.class_ "switch" ]
    [ HH.input [ HP.type_ HP.InputCheckbox, HP.id id, HP.checked checked, HE.onChecked onChange ]
    , HH.span [ HS.class_ "switch-track" ] []
    , HH.span [ HS.class_ "switch-text" ] $
        [ HH.text text ]
        <> maybe [] (\note' -> [ HH.span [ HS.class_ "switch-note" ] [ HH.text note' ] ]) note
    ]

switches :: ∀ w i. Array (HH.HTML w i) -> HH.HTML w i
switches = HH.div [ HS.class_ "switches" ]
