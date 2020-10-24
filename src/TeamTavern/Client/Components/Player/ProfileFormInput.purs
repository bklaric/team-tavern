module TeamTavern.Client.Components.Player.ProfileFormInput (Input, Output, Slot, emptyInput, profileFormInput) where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Input (inputGroupsHeading, responsiveInputGroups)
import TeamTavern.Client.Components.Player.ProfileInputGroup (ChildSlots, Field, FieldValue, ambitionsInputGroup, fieldInputGroup, newOrReturningInputGroup)

type Input =
    { fields :: Array Field
    , fieldValues :: Array FieldValue
    , newOrReturning :: Boolean
    , ambitions :: String
    , urlErrors :: Array String
    , missingErrors :: Array String
    , ambitionsError :: Boolean
    }

type Output =
    { fieldValues :: Array FieldValue
    , ambitions :: String
    , newOrReturning :: Boolean
    }

type State =
    { fields :: Array Field
    , fieldValues :: Array FieldValue
    , newOrReturning :: Boolean
    , ambitions :: String
    , urlErrors :: Array String
    , missingErrors :: Array String
    , ambitionsError :: Boolean
    }

data Action
    = Receive Input
    | UpdateUrl String (Maybe String)
    | UpdateSingleSelect String (Maybe String)
    | UpdateMultiSelect String (Array String)
    | UpdateNewOrReturning Boolean
    | UpdateAmbitions String

type Slot = H.Slot (Const Void) Output Unit

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { fields, ambitions, fieldValues, newOrReturning, urlErrors, missingErrors, ambitionsError }
    = HH.div_ $
    [ inputGroupsHeading "Details"
    , responsiveInputGroups $
        ( fields <#> fieldInputGroup
            fieldValues UpdateUrl UpdateSingleSelect UpdateMultiSelect urlErrors missingErrors
        )
        <>
        [ newOrReturningInputGroup newOrReturning UpdateNewOrReturning ]
    , inputGroupsHeading "Ambitions"
    , ambitionsInputGroup ambitions UpdateAmbitions ambitionsError
    ]

raiseOutput :: forall left. State -> H.HalogenM State Action ChildSlots Output (Async left) Unit
raiseOutput { fieldValues, newOrReturning, ambitions } =
    H.raise { fieldValues, newOrReturning, ambitions }

handleAction :: forall left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive input) =
    H.put input
handleAction (UpdateUrl fieldKey url) = do
    state <- H.modify \state -> state
        { fieldValues = state.fieldValues <#> \fieldValue ->
            if fieldValue.fieldKey == fieldKey
            then fieldValue { url = url }
            else fieldValue
        }
    raiseOutput state
handleAction (UpdateSingleSelect fieldKey optionKey) = do
    state <- H.modify \state -> state
        { fieldValues = state.fieldValues <#> \fieldValue ->
            if fieldValue.fieldKey == fieldKey
            then fieldValue { optionKey = optionKey }
            else fieldValue
        }
    raiseOutput state
handleAction (UpdateMultiSelect fieldKey optionKeys) = do
    state <- H.modify \state -> state
        { fieldValues = state.fieldValues <#> \fieldValue ->
            if fieldValue.fieldKey == fieldKey
            then fieldValue { optionKeys = Just optionKeys }
            else fieldValue
        }
    raiseOutput state
handleAction (UpdateNewOrReturning newOrReturning) = do
    state <- H.modify _ { newOrReturning = newOrReturning }
    raiseOutput state
handleAction (UpdateAmbitions ambitions) = do
    state <- H.modify _ { ambitions = ambitions }
    raiseOutput state

component :: forall query left. H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

emptyInput :: Array Field -> Input
emptyInput fields =
    { fields
    , fieldValues: []
    , newOrReturning: false
    , ambitions: ""
    , urlErrors: []
    , missingErrors: []
    , ambitionsError: false
    }

profileFormInput
    :: forall action children left
    .  Input
    -> (Output -> Maybe action)
    -> HH.ComponentHTML action (profileFormInput :: Slot | children) (Async left)
profileFormInput input handleMessage =
    HH.slot (SProxy :: SProxy "profileFormInput") unit component input handleMessage
