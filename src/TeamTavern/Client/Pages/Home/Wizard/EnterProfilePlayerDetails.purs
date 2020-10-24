module TeamTavern.Client.Pages.Home.Wizard.EnterProfilePlayerDetails where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (find)
import Data.Maybe (Maybe(..), maybe)
import Data.Variant (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.SelectDefinitive.MultiSelect (multiSelectIndexed)
import TeamTavern.Client.Components.SelectDefinitive.MultiSelect as MultiSelect
import TeamTavern.Client.Components.SelectDefinitive.SingleSelect (singleSelectIndexed)
import TeamTavern.Client.Components.SelectDefinitive.SingleSelect as SingleSelect
import TeamTavern.Client.Snippets.ErrorClasses (inputErrorClass)

type Option =
    { key :: String
    , label :: String
    }

type Field =
    { key :: String
    , ilk :: Int
    , label :: String
    , icon :: String
    , required :: Boolean
    , domain :: Maybe String
    , options :: Maybe (Array Option)
    }

type FieldValue =
    { fieldKey :: String
    , url :: Maybe String
    , optionKey :: Maybe String
    , optionKeys :: Maybe (Array String)
    }

type Input =
    { fields :: Array Field
    , fieldValues :: Array FieldValue
    , newOrReturning :: Boolean
    , summary :: String
    , urlErrors :: Array String
    , missingErrors :: Array String
    , summaryError :: Boolean
    }

type Output =
    { fieldValues :: Array FieldValue
    , summary :: String
    , newOrReturning :: Boolean
    }

type State =
    { fields :: Array Field
    , fieldValues :: Array FieldValue
    , summary :: String
    , newOrReturning :: Boolean
    , summaryError :: Boolean
    , urlErrors :: Array String
    , missingErrors :: Array String
    }

data Action
    = UpdateUrl String String
    | UpdateSingleSelect String (Maybe String)
    | UpdateMultiSelect String (Array String)
    | UpdateNewOrReturning Boolean
    | UpdateSummary String

type ChildSlots =
    ( "singleSelectField" :: SingleSelect.Slot Option String
    , "multiSelectField" :: MultiSelect.Slot Option String
    )

type Slot = H.Slot (Const Void) Output Unit

fieldLabel :: forall slots action.
    String -> String -> Boolean -> Maybe String -> HH.HTML slots action
fieldLabel label icon required domain =
    HH.label
        [ HP.class_ $ HH.ClassName "input-label" ] $
        [ HH.i [ HP.class_ $ HH.ClassName $ icon <> " filter-field-icon" ] []
        , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text label ]
        ]
        <>
        (case domain of
        Just domain' ->
            [ divider
            , HH.span [ HP.class_ $ H.ClassName "input-sublabel" ] [ HH.text domain' ]
            ]
        Nothing -> [])
        <>
        (if required
        then
            [ divider
            , HH.span [ HP.class_ $ H.ClassName "input-sublabel" ] [ HH.text "required" ]
            ]
        else
            [])

fieldInput
    :: forall left
    .  Array FieldValue
    -> Array String
    -> Array String
    -> Field
    -> H.ComponentHTML Action ChildSlots (Async left)
fieldInput fieldValues urlValueErrors missingErrors { key, ilk: 1, label, icon, required, domain: Just domain } = let
    fieldValue' = fieldValues # find \{ fieldKey } -> fieldKey == key
    urlError = urlValueErrors # Array.any (_ == key)
    missingError = missingErrors # Array.any (_ == key)
    url = fieldValue' >>= _.url
    in
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel label icon required (Just domain)
    , HH.input
        [ HP.id_ key
        , HP.class_ $ HH.ClassName "text-line-input"
        , HE.onValueInput $ Just <<< UpdateUrl key
        , HP.value $ maybe "" identity url
        ]
    , HH.p
        [ HP.class_ $ inputErrorClass urlError ]
        [ HH.text $ "This doesn't look like a valid " <> label <> " (" <> domain <> ") address." ]
    , HH.p
        [ HP.class_ $ inputErrorClass missingError ]
        [ HH.text $ label <> " is required." ]
    ]
fieldInput fieldValues _ _ { key, ilk: 2, label, icon, required, options: Just options } = let
    fieldValue' = fieldValues # find \{ fieldKey } -> fieldKey == key
    in
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel label icon required Nothing
    , singleSelectIndexed (SProxy :: SProxy "singleSelectField") key
        { options
        , selected: fieldValue' >>= _.optionKey >>= \optionKey ->
            options # find \option -> optionKey == option.key
        , labeler: _.label
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        , filter: Nothing
        }
        \option -> Just $ UpdateSingleSelect key (option <#> _.key)
    ]
fieldInput fieldValues _ _ { key, ilk: 3, label, icon, required, options: Just options } = let
    fieldValue' = fieldValues # find \{ fieldKey } -> fieldKey == key
    selectedOptionKeys' = fieldValue' >>= _.optionKeys
    in
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel label icon required Nothing
    , multiSelectIndexed (SProxy :: SProxy "multiSelectField") key
        { options: options
        , selected:  selectedOptionKeys' # maybe [] \selectedOptionKeys ->
            options # Array.filter (\option -> Array.any (_ == option.key) selectedOptionKeys)
        , labeler: _.label
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        , filter: Nothing
        }
        \options' -> Just $ UpdateMultiSelect key (options' <#> _.key)
    ]
fieldInput _ _ _ _ = HH.div_ []

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render
    { fields
    , summary
    , fieldValues
    , newOrReturning
    , urlErrors
    , missingErrors
    , summaryError
    } =
    HH.div_ $
    [ HH.div [ HP.class_ $ HH.ClassName "responsive-input-groups" ] $
        (fields <#> fieldInput fieldValues urlErrors missingErrors)
        <>
        [ HH.div [ HP.class_ $ HH.ClassName "input-group" ]
            [ fieldLabel "New or returning player" "fas fa-book" false Nothing
            , HH.label
                [ HP.class_ $ HH.ClassName "checkbox-input-label" ]
                [ HH.input
                    [ HP.class_ $ HH.ClassName "checkbox-input"
                    , HP.type_ HP.InputCheckbox
                    , HP.checked newOrReturning
                    , HE.onChecked (Just <<< UpdateNewOrReturning)
                    ]
                , HH.text "I'm a new or returning player to the game."
                ]
            ]
        ]
    , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
        [ HH.label
            [ HP.class_ $ HH.ClassName "input-label"
            , HP.for "summary"
            ]
            [ HH.text "Summary"
            , divider
            , HH.span [ HP.class_ $ H.ClassName "input-sublabel" ] [ HH.text "required" ]
            ]
        , HH.textarea
            [ HP.id_ "summary"
            , HP.class_ $ HH.ClassName "text-input"
            , HE.onValueInput $ Just <<< UpdateSummary
            , HP.value summary
            ]
        , HH.p
            [ HP.class_ $ inputErrorClass summaryError ]
            [ HH.text
                "The summary cannot be empty and cannot be more than 2000 characters long." ]
        ]
    ]

raiseMessage :: forall left.
    State -> H.HalogenM State Action ChildSlots Output (Async left) Unit
raiseMessage { fieldValues, newOrReturning, summary } =
    H.raise { fieldValues, newOrReturning, summary }

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (UpdateUrl fieldKey url) = do
    state <- H.modify \state -> state
        { fieldValues = state.fieldValues <#> \fieldValue ->
            if fieldValue.fieldKey == fieldKey
            then fieldValue { url = Just url }
            else fieldValue
        }
    raiseMessage state
handleAction (UpdateSingleSelect fieldKey optionKey) = do
    state <- H.modify \state -> state
        { fieldValues = state.fieldValues <#> \fieldValue ->
            if fieldValue.fieldKey == fieldKey
            then fieldValue { optionKey = optionKey }
            else fieldValue
        }
    raiseMessage state
handleAction (UpdateMultiSelect fieldKey optionKeys) = do
    state <- H.modify \state -> state
        { fieldValues = state.fieldValues <#> \fieldValue ->
            if fieldValue.fieldKey == fieldKey
            then fieldValue { optionKeys = Just optionKeys }
            else fieldValue
        }
    raiseMessage state
handleAction (UpdateNewOrReturning newOrReturning) = do
    state <- H.modify (_ { newOrReturning = newOrReturning })
    raiseMessage state
handleAction (UpdateSummary summary) = do
    state <- H.modify _ { summary = summary }
    raiseMessage state

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: \input -> input
        { fieldValues = input.fields <#> \field ->
            case input.fieldValues # find \fieldValue -> fieldValue.fieldKey == field.key of
            Just fieldValue -> fieldValue
            Nothing ->
                { fieldKey: field.key
                , url: Nothing
                , optionKey: Nothing
                , optionKeys: Nothing
                }
        }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

enterProfilePlayerDetails
    :: forall action children left
    .  Input
    -> (Output -> Maybe action)
    -> HH.ComponentHTML action (enterProfilePlayerDetails :: Slot | children) (Async left)
enterProfilePlayerDetails input handleMessage =
    HH.slot (SProxy :: SProxy "enterProfilePlayerDetails") unit component input handleMessage
