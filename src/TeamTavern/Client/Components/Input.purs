module TeamTavern.Client.Components.Input where

import Prelude

import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Script.Request (justIfInt, nothingIfEmpty)
import TeamTavern.Client.Snippets.Class as HS

inputLabel' :: forall slots action.
    String -> String -> Maybe String -> Boolean -> HH.HTML slots action
inputLabel' icon label domain required =
    HH.label
    [ HS.class_ "input-label"] $
    [ HH.i [ HS.class_ $ icon <> " input-label-icon" ] []
    , HH.span [ HS.class_ "input-label-text" ] [ HH.text label ]
    ]
    <>
    (case domain of
    Just domain' ->
        [ divider
        , HH.span [ HP.class_ $ H.ClassName "input-sublabel" ] [ HH.text domain' ]
        ]
    Nothing -> []
    )
    <>
    if required
    then
        [ divider
        , HH.span [ HP.class_ $ H.ClassName "input-primary-sublabel" ] [ HH.text "required" ]
        ]
    else []

requiredDomainInputLabel :: forall slots action. String -> String -> String -> HH.HTML slots action
requiredDomainInputLabel icon label domain = inputLabel' icon label (Just domain) true

domainInputLabel :: forall slots action. String -> String -> String -> HH.HTML slots action
domainInputLabel icon label domain = inputLabel' icon label (Just domain) false

requiredInputLabel :: forall slots action. String -> String -> HH.HTML slots action
requiredInputLabel icon label = inputLabel' icon label Nothing true

inputLabel :: forall slots action. String -> String -> HH.HTML slots action
inputLabel icon label = inputLabel' icon label Nothing false

inputUnderlabel :: forall slots action. String -> HH.HTML slots action
inputUnderlabel text =
    HH.label
    [ HS.class_ "input-underlabel" ]
    [ HH.text text ]

inputError :: forall slots action. Boolean -> String -> Array (HH.HTML slots action)
inputError true text = [ HH.p [ HS.class_ "input-error" ] [ HH.text text ] ]
inputError false text = []

inputGroup :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
inputGroup group = HH.div [ HS.class_ "input-group" ] group

responsiveInputGroups :: forall slots action. Array (HH.HTML slots action) -> HH.HTML slots action
responsiveInputGroups groups = HH.div [ HS.class_ "responsive-input-groups" ] groups

inputGroupsHeading :: forall slots action. String -> HH.HTML slots action
inputGroupsHeading text = HH.h3 [ HS.class_ "input-groups-heading" ] [ HH.text text ]

requiredTextLineInput :: forall slots action. String -> (String -> action) -> HH.HTML slots action
requiredTextLineInput input onInput =
    HH.input
    [ HS.class_ "text-line-input"
    , HP.type_ HP.InputText
    , HP.value input
    , HE.onValueInput $ Just <<< onInput
    ]

textLineInput :: forall slots action.
    Maybe String -> (Maybe String -> action) -> HH.HTML slots action
textLineInput input onInput =
    requiredTextLineInput (maybe "" identity input) (onInput <<< nothingIfEmpty)

textInput :: forall slots action. String -> String -> (String -> action) -> HH.HTML slots action
textInput placeholder input onInput =
    HH.textarea
    [ HS.class_ "text-input"
    , HP.placeholder placeholder
    , HP.value input
    , HE.onValueInput $ Just <<< onInput
    ]

textInput_ :: forall slots action. String -> (String -> action) -> HH.HTML slots action
textInput_ = textInput ""

checkboxInput :: forall slots action.
    Boolean -> (Boolean -> action) -> String -> HH.HTML slots action
checkboxInput input onInput label =
    HH.label
    [ HS.class_ "checkbox-input-label" ]
    [ HH.input
        [ HS.class_ "checkbox-input"
        , HP.type_ HP.InputCheckbox
        , HP.checked input
        , HE.onChecked $ Just <<< onInput
        ]
    , HH.text label
    ]

numberInput :: forall slots action. Maybe Int -> (Maybe Int -> action) -> HH.HTML slots action
numberInput (input :: Maybe Int) onInput =
    HH.input
    [ HS.class_ $ "range-input-part"
    , HP.type_ HP.InputNumber
    , HP.value $ maybe "" show input
    , HE.onValueChange $ Just <<< onInput <<< justIfInt
    ]

numberRangeInput :: forall slots action.
    Maybe Int -> Maybe Int -> (Maybe Int -> action) -> (Maybe Int -> action) -> HH.HTML slots action
numberRangeInput fromInput toInput onFromInput onToInput =
    HH.div
    [ HS.class_ "range-input" ]
    [ HH.span [ HS.class_ "range-input-from" ] [ HH.text "From" ]
    , numberInput fromInput onFromInput
    , HH.span [ HS.class_ "range-input-to" ] [ HH.text "to" ]
    , numberInput toInput onToInput
    ]

timeInput :: forall slots action.
    Boolean -> Maybe String -> (Maybe String -> action) -> HH.HTML slots action
timeInput disabled input onInput =
    HH.input
    [ HS.class_ $ "range-input-part"
    , HP.type_ HP.InputTime
    , HP.disabled disabled
    , HP.value $ maybe "" identity input
    , HE.onValueChange $ Just <<< onInput <<< nothingIfEmpty
    ]

timeRangeInput
    :: forall slots action
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

timeRangeInputUnderlabel :: forall slots action.
    Boolean -> Maybe String -> Maybe String -> Array (HH.HTML slots action)
timeRangeInputUnderlabel disabled fromInput toInput =
    if disabled
    then [ inputUnderlabel "Set your timezone to unlock this field." ]
    else if isNothing fromInput && isJust toInput || isJust fromInput && isNothing toInput
    then [ inputUnderlabel "Enter both times for the field to have effect." ]
    else []