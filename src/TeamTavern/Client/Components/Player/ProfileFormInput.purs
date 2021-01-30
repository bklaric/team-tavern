module TeamTavern.Client.Components.Player.ProfileFormInput (FieldValues, Input, Output, Slot, emptyInput, profileFormInput) where

import Prelude

import Async (Async)
import Data.Array (foldl)
import Data.Array as Array
import Data.Const (Const)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Variant (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Record as Record
import TeamTavern.Client.Components.Input (externalIdHeading, inputGroupsHeading, responsiveInputGroups)
import TeamTavern.Client.Components.Player.ProfileInputGroup (ChildSlots, Field, FieldValue, ambitionsInputGroup, externalIdInputGroup, fieldInputGroup, newOrReturningInputGroup)
import TeamTavern.Client.Components.Player.ProfileInputGroup as Input
import TeamTavern.Routes.Shared.ExternalIdIlk (ExternalIdIlk, ExternalIdIlks)

type FieldValues = Array FieldValue

type Input =
    { externalIdIlks :: ExternalIdIlks
    , fields :: Array Field
    , externalIdIlk :: ExternalIdIlk
    , externalId :: String
    , fieldValues :: FieldValues
    , newOrReturning :: Boolean
    , ambitions :: String
    , externalIdError :: Boolean
    , urlErrors :: Array String
    , ambitionsError :: Boolean
    }

type Output =
    { externalIdIlk :: ExternalIdIlk
    , externalId :: String
    , externalIdError :: Boolean
    , fieldValues :: FieldValues
    , ambitions :: String
    , newOrReturning :: Boolean
    }

type State =
    { externalIdIlks :: ExternalIdIlks
    , fields :: Array Field
    , externalIdIlk :: ExternalIdIlk
    , externalId :: String
    , fieldValues :: Input.FieldValues
    , newOrReturning :: Boolean
    , ambitions :: String
    , externalIdError :: Boolean
    , urlErrors :: Array String
    , ambitionsError :: Boolean
    }

data Action
    = Receive Input
    | UpdateExternalIdIlk ExternalIdIlk
    | UpdateExternalId String
    | UpdateUrl String (Maybe String)
    | UpdateSingleSelect String (Maybe String)
    | UpdateMultiSelect String (Array String)
    | UpdateNewOrReturning Boolean
    | UpdateAmbitions String

type Slot = H.Slot (Const Void) Output Unit

fieldValuesToArray :: forall key value. Map key value -> Array value
fieldValuesToArray = Array.fromFoldable <<< Map.values

fieldValuesToMap :: forall fields.
    Array { fieldKey :: String | fields } -> Map String { fieldKey :: String | fields }
fieldValuesToMap = foldl (\map value -> Map.insert value.fieldKey value map) Map.empty

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render
    { externalIdIlks, fields
    , externalIdIlk, externalId, fieldValues, newOrReturning, ambitions
    , externalIdError, urlErrors, ambitionsError
    }
    = HH.div_ $
    [ externalIdHeading externalIdIlks externalIdIlk UpdateExternalIdIlk
    , responsiveInputGroups
        [ externalIdInputGroup externalIdIlk externalId UpdateExternalId externalIdError ]
    , inputGroupsHeading "Details"
    , responsiveInputGroups $
        ( fields <#> fieldInputGroup fieldValues
            UpdateUrl UpdateSingleSelect UpdateMultiSelect urlErrors
        )
        <>
        [ newOrReturningInputGroup newOrReturning UpdateNewOrReturning ]
    , inputGroupsHeading "Ambitions"
    , ambitionsInputGroup ambitions UpdateAmbitions ambitionsError
    ]

raiseOutput :: forall left. State -> H.HalogenM State Action ChildSlots Output (Async left) Unit
raiseOutput { externalIdIlk, externalId, externalIdError, fieldValues, newOrReturning, ambitions } =
    H.raise
    { externalIdIlk, externalId, externalIdError
    , fieldValues: fieldValuesToArray fieldValues, newOrReturning, ambitions
    }

handleAction :: forall left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive input) =
    H.put (Record.modify (SProxy :: SProxy "fieldValues") fieldValuesToMap input)
handleAction (UpdateExternalIdIlk externalIdIlk) = do
    state <- H.modify _
        { externalIdIlk = externalIdIlk
        , externalId = ""
        , externalIdError = false
        }
    raiseOutput state
handleAction (UpdateExternalId externalId) = do
    state <- H.modify _ { externalId = externalId }
    raiseOutput state
handleAction (UpdateUrl fieldKey url) = do
    state <- H.modify \state -> state
        { fieldValues =
            case url of
            Nothing -> Map.delete fieldKey state.fieldValues
            Just url' ->
                Map.insert
                fieldKey
                { fieldKey, url: Just url', optionKey: Nothing, optionKeys: Nothing }
                state.fieldValues
        }
    raiseOutput state
handleAction (UpdateSingleSelect fieldKey optionKey) = do
    state <- H.modify \state -> state
        { fieldValues =
            case optionKey of
            Nothing -> Map.delete fieldKey state.fieldValues
            Just optionKey' ->
                Map.insert
                fieldKey
                { fieldKey, url: Nothing, optionKey: Just optionKey', optionKeys: Nothing }
                state.fieldValues
        }
    raiseOutput state
handleAction (UpdateMultiSelect fieldKey optionKeys) = do
    state <- H.modify \state -> state
        { fieldValues =
            case Array.uncons optionKeys of
            Nothing -> Map.delete fieldKey state.fieldValues
            Just { head, tail } ->
                Map.insert
                fieldKey
                { fieldKey
                , url: Nothing
                , optionKey: Nothing
                , optionKeys: Just $ Array.cons head tail
                }
                state.fieldValues
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
    { initialState: Record.modify (SProxy :: SProxy "fieldValues") fieldValuesToMap
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

emptyInput :: forall props.
    { externalIdIlks :: ExternalIdIlks, fields :: Array Field | props } -> Input
emptyInput { externalIdIlks, fields } =
    { externalIdIlks
    , fields
    , externalIdIlk: externalIdIlks.head
    , externalId: ""
    , fieldValues: []
    , newOrReturning: false
    , ambitions: ""
    , externalIdError: false
    , urlErrors: []
    , ambitionsError: false
    }

profileFormInput
    :: forall action children left
    .  Input
    -> (Output -> action)
    -> HH.ComponentHTML action (playerProfileFormInput :: Slot | children) (Async left)
profileFormInput input handleMessage =
    HH.slot (SProxy :: SProxy "playerProfileFormInput") unit component input (Just <<< handleMessage)
