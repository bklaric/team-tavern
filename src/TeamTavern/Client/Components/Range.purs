module TeamTavern.Client.Components.Range
    ( ageRange
    , hourOptions
    , hoursHint
    , hoursRange
    , optionRange
    ) where

import Prelude

import Data.Array (range)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import TeamTavern.Client.Components.Input (Option, input, select)
import TeamTavern.Client.Script.Clock (clock)
import TeamTavern.Client.Snippets.Class as HS

-- `name` is the field's, such as "Rank range", which names the two ends
-- together, so that "Lowest" is read as the lowest rank.
type Range i = { name :: String, from :: String, to :: String, onFrom :: String -> i, onTo :: String -> i }

-- Two ends with "to" between them. Either end may be left empty.
range' :: ∀ w i. String -> HH.HTML w i -> HH.HTML w i -> HH.HTML w i
range' name from to =
    HH.div [ HS.class_ "range", HPA.role "group", HPA.label name ]
    [ from, HH.span [ HS.class_ "muted" ] [ HH.text "to" ], to ]

-- A range over a field's ordered options, such as ranks, lowest first.
optionRange :: ∀ w i. Array Option -> Range i -> HH.HTML w i
optionRange options { name, from, to, onFrom, onTo } =
    range' name
    (select [ HPA.label "Lowest" ] { options, value: from, placeholder: Just "Lowest", onChange: onFrom })
    (select [ HPA.label "Highest" ] { options, value: to, placeholder: Just "Highest", onChange: onTo })

-- Ages a player may be, in years.
ageRange :: ∀ w i. Range i -> HH.HTML w i
ageRange { name, from, to, onFrom, onTo } =
    range' name (age "From" from onFrom) (age "To" to onTo)
    where
    age label value onInput =
        input
        [ HP.type_ HP.InputNumber
        , HP.min 13.0
        , HP.max 99.0
        , HP.attr (HH.AttrName "inputmode") "numeric"
        , HPA.label label
        ]
        { value, placeholder: label, onInput }

-- The hours of the day, "19:00", labelled as the viewer's locale writes them.
hourOptions :: Array Option
hourOptions = range 0 23 <#> \hour ->
    { value: (if hour < 10 then "0" else "") <> show hour <> ":00", label: clock $ hour * 60 }

-- The hint the field holding an hours range gives.
hoursHint :: String
hoursHint = "In your own time. A range can cross midnight."

-- When a player is online, from one hour to another; to can come before from,
-- since the range can cross midnight.
hoursRange :: ∀ w i. Range i -> HH.HTML w i
hoursRange { name, from, to, onFrom, onTo } =
    range' name
    (select [ HPA.label "From" ] { options: hourOptions, value: from, placeholder: Just "From", onChange: onFrom })
    (select [ HPA.label "To" ] { options: hourOptions, value: to, placeholder: Just "To", onChange: onTo })
