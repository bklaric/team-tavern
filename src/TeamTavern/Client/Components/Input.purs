module TeamTavern.Client.Components.Input where

import Prelude

import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.Monoid (guard)
import Data.String (length)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Checkable (checkbox)
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Script.Request (justIfInt, nothingIfEmpty)
import TeamTavern.Client.Snippets.Class as HS
import Unsafe.Coerce (unsafeCoerce)

inputLabel' :: ∀ slots action.
    String -> String -> Maybe String -> Boolean -> HH.HTML slots action
inputLabel' icon label domain required =
    HH.label
    [ HS.class_ "input-label"] $
    [ HH.i [ HS.class_ $ icon <> " input-label-icon" ] []
    , HH.span [ HS.class_ "input-label-text" ] [ HH.text label ]
    ]
    <>
    (case domain of
    Just domain' -> [ divider, inputSublabel domain' ]
    Nothing -> []
    )
    <>
    if required
    then [ divider, inputRequiredSublabel ]
    else []

requiredDomainInputLabel :: ∀ slots action. String -> String -> String -> HH.HTML slots action
requiredDomainInputLabel icon label domain = inputLabel' icon label (Just domain) true

domainInputLabel :: ∀ slots action. String -> String -> String -> HH.HTML slots action
domainInputLabel icon label domain = inputLabel' icon label (Just domain) false

requiredInputLabel :: ∀ slots action. String -> String -> HH.HTML slots action
requiredInputLabel icon label = inputLabel' icon label Nothing true

inputLabel :: ∀ slots action. String -> String -> HH.HTML slots action
inputLabel icon label = inputLabel' icon label Nothing false

inputLabel_ :: ∀ slots action. String -> HH.HTML slots action
inputLabel_ label = HH.label [ HS.class_ "input-label" ] [ HH.text label ]

platformIdLabel :: ∀ slots action.
    HH.HTML slots action -> String -> Boolean -> HH.HTML slots action
platformIdLabel icon label required =
    HH.label
    [ HS.class_ "input-label"] $
    [ icon
    , HH.span [ HS.class_ "input-label-text" ] [ HH.text label ]
    ]
    <>
    guard required [ divider, inputRequiredSublabel ]

inputSublabel :: ∀ slots action. String -> HH.HTML slots action
inputSublabel text = HH.span [ HS.class_ "input-sublabel" ] [ HH.text text ]

inputPrimarySublabel :: ∀ slots action. String -> HH.HTML slots action
inputPrimarySublabel text = HH.span [ HS.class_ "input-primary-sublabel" ] [ HH.text text ]

inputRequiredSublabel :: ∀ slots action. HH.HTML slots action
inputRequiredSublabel = inputPrimarySublabel "Required"

inputErrorSublabel :: ∀ slots action. String -> HH.HTML slots action
inputErrorSublabel text = HH.span [ HS.class_ "input-error-sublabel" ] [ HH.text text ]

inputUnderlabel' :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
inputUnderlabel' children = HH.label [ HS.class_ "input-underlabel" ] children

inputUnderlabel :: ∀ slots action. String -> HH.HTML slots action
inputUnderlabel text = inputUnderlabel' [ HH.text text ]

inputError :: ∀ slots action. Boolean -> String -> Array (HH.HTML slots action)
inputError error text = guard error [ HH.p [ HS.class_ "input-error" ] [ HH.text text ] ]

textInputSubcontent :: forall slots action. String -> Boolean -> String -> Array (HH.HTML slots action)
textInputSubcontent textName error text =
    let textLength = length text
    in
    guard (error || (textLength > 1500))
    [ HH.div
        [HS.class_ "text-input-subcontent"]
        (inputError error (textName <> " text cannot be more than 2000 characters long.")
        <> guard (textLength > 1500) [inputUnderlabel (show textLength <> "/2000")])
    ]

aboutInputSubcontent :: forall slots action. Boolean -> String -> Array (HH.HTML slots action)
aboutInputSubcontent = textInputSubcontent "About"

ambitionsInputSubcontent :: forall slots action. Boolean -> String -> Array (HH.HTML slots action)
ambitionsInputSubcontent = textInputSubcontent "Ambitions"

inputGroup :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
inputGroup group = HH.div [ HS.class_ "input-group" ] group

responsiveInputGroups :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
responsiveInputGroups groups = HH.div [ HS.class_ "responsive-input-groups" ] groups

inputGroupsHeading' :: ∀ slots action. Array (HH.HTML slots action) -> HH.HTML slots action
inputGroupsHeading' children = HH.h2 [ HS.class_ "input-groups-heading" ] children

inputGroupsHeading :: ∀ slots action. String -> HH.HTML slots action
inputGroupsHeading text = inputGroupsHeading' [ HH.text text ]

requiredTextLineInput :: ∀ slots action. String -> (String -> action) -> HH.HTML slots action
requiredTextLineInput input onInput =
    HH.input
    [ HS.class_ "text-line-input"
    , HP.type_ HP.InputText
    , HP.value input
    , HE.onValueInput onInput
    ]

requiredTextLineInputNamed :: ∀ slots action. String -> String -> (String -> action) -> HH.HTML slots action
requiredTextLineInputNamed name input onInput =
    HH.input
    [ HS.class_ "text-line-input"
    , HP.type_ HP.InputText
    , HP.name name
    , HP.value input
    , HE.onValueInput onInput
    ]

textLineInput :: ∀ slots action.
    Maybe String -> (Maybe String -> action) -> HH.HTML slots action
textLineInput input onInput =
    requiredTextLineInput (maybe "" identity input) (onInput <<< nothingIfEmpty)

textInput :: ∀ slots action. String -> String -> (String -> action) -> HH.HTML slots action
textInput placeholder input onInput =
    HH.textarea
    [ HS.class_ "text-input"
    , HP.placeholder placeholder
    , HP.value input
    , HE.onValueInput onInput
    ]

textInput_ :: ∀ slots action. String -> (String -> action) -> HH.HTML slots action
textInput_ = textInput ""

numberInput :: ∀ slots action. Maybe Int -> (Maybe Int -> action) -> HH.HTML slots action
numberInput (input :: Maybe Int) onInput =
    HH.input
    [ HS.class_ $ "range-input-part"
    , HP.type_ HP.InputNumber
    , HP.value $ maybe "" show input
    , HE.onValueChange $ onInput <<< justIfInt
    ]

numberRangeInput :: ∀ slots action.
    Maybe Int -> Maybe Int -> (Maybe Int -> action) -> (Maybe Int -> action) -> HH.HTML slots action
numberRangeInput fromInput toInput onFromInput onToInput =
    HH.div
    [ HS.class_ "range-input" ]
    [ HH.span [ HS.class_ "range-input-from" ] [ HH.text "From" ]
    , numberInput fromInput onFromInput
    , HH.span [ HS.class_ "range-input-to" ] [ HH.text "to" ]
    , numberInput toInput onToInput
    ]

timeInput :: ∀ slots action.
    Boolean -> Maybe String -> (Maybe String -> action) -> HH.HTML slots action
timeInput disabled input onInput =
    HH.input
    [ HS.class_ $ "range-input-part"
    , HP.type_ HP.InputTime
    , HP.disabled disabled
    , HP.value $ maybe "" identity input
    , HE.onValueChange $ onInput <<< nothingIfEmpty
    ]

timeRangeInput
    :: ∀ slots action
    .  Boolean
    -> Maybe String
    -> Maybe String
    -> (Maybe String -> action)
    -> (Maybe String -> action)
    -> HH.HTML slots action
timeRangeInput disabled fromInput toInput onFromInput onToInput =
    HH.div
    [ HS.class_ "range-input" ]
    [ HH.span [ HS.class_ "range-input-from" ] [ HH.text "From" ]
    , timeInput disabled fromInput onFromInput
    , HH.span [ HS.class_ "range-input-to" ] [ HH.text "to" ]
    , timeInput disabled toInput onToInput
    ]

timeRangeInputUnderlabel :: ∀ slots action.
    Boolean -> Maybe String -> Maybe String -> Array (HH.HTML slots action)
timeRangeInputUnderlabel disabled fromInput toInput =
    if disabled
    then [ inputUnderlabel "Set your timezone to unlock this field." ]
    else if isNothing fromInput && isJust toInput || isJust fromInput && isNothing toInput
    then [ inputUnderlabel "Enter both times for the field to have effect." ]
    else []

dateInput :: ∀ slots action.
    String -> String -> (Maybe String) -> (Maybe String -> action) -> HH.HTML slots action
dateInput min max value onValue =
    HH.input
    [ HS.class_ "text-line-input"
    , HP.type_ HP.InputDate
    , HP.min $ unsafeCoerce min
    , HP.max $ unsafeCoerce max
    , HP.value $ maybe "" identity value
    , HE.onValueInput $ onValue <<< nothingIfEmpty
    ]

checkboxLabel :: ∀ slots action. String -> HH.HTML slots action
checkboxLabel label = HH.span [ HS.class_ "checkbox-input-label" ] [ HH.text label ]

checkboxInput :: ∀ slots action.
    Boolean -> (Boolean -> action) -> String -> HH.HTML slots action
checkboxInput checked onChecked label =
    HH.div
    [ HS.class_ "checkbox-input"
    , HE.onClick $ const $ onChecked $ not checked
    ]
    [ checkbox checked
    , checkboxLabel label
    ]
