module TeamTavern.Client.Components.Input
    ( Option
    , input
    , select
    , textarea
    ) where

import Prelude

import Data.Maybe (Maybe, maybe)
import DOM.HTML.Indexed (HTMLinput, HTMLselect, HTMLtextarea)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Snippets.Class as HS

type Option = { value :: String, label :: String }

-- The props given first, such as the id and the type, go on the control
-- itself.
input :: ∀ w i. Array (HP.IProp HTMLinput i) ->
    { value :: String, placeholder :: String, onInput :: String -> i } -> HH.HTML w i
input props { value, placeholder, onInput } =
    HH.input $
    [ HS.class_ "input"
    , HP.value value
    , HP.placeholder placeholder
    , HP.autocomplete HP.AutocompleteOff
    , HE.onValueInput onInput
    ]
    <> props

-- The empty value is the placeholder's, which is offered first when there is
-- one.
select :: ∀ w i. Array (HP.IProp HTMLselect i) ->
    { options :: Array Option, value :: String, placeholder :: Maybe String, onChange :: String -> i } -> HH.HTML w i
select props { options, value, placeholder, onChange } =
    HH.select ([ HS.class_ "select", HE.onValueChange onChange ] <> props) $
    maybe [] (\placeholder' -> [ option { value: "", label: placeholder' } ]) placeholder
    <> (options <#> option)
    where
    option option' =
        HH.option [ HP.value option'.value, HP.selected $ option'.value == value ] [ HH.text option'.label ]

-- What to write is prompted by the field's hint, "Ideas: …", and the
-- placeholder gives an example.
textarea :: ∀ w i. Array (HP.IProp HTMLtextarea i) ->
    { value :: String, placeholder :: String, onInput :: String -> i } -> HH.HTML w i
textarea props { value, placeholder, onInput } =
    HH.textarea $
    [ HS.class_ "textarea"
    , HP.rows 5
    , HP.value value
    , HP.placeholder placeholder
    , HE.onValueInput onInput
    ]
    <> props
