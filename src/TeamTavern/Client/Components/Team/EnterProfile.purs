module TeamTavern.Client.Components.Team.EnterProfile (Input, Output, Slot, emptyInput, enterProfile) where

import Prelude

import Async (Async)
import Data.Array as Array
import Data.Const (Const)
import Data.List as List
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.MultiMap as MultiMap
import Data.NonEmpty (NonEmpty(..))
import Data.Variant (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Record as Record
import TeamTavern.Client.Components.Input (inputGroupsHeading, responsiveInputGroups)
import TeamTavern.Client.Components.SelectDefinitive.MultiSelect as MultiSelect
import TeamTavern.Client.Components.Team.ProfileInputGroup (Field, FieldValues, Option, ambitionsInputGroup, fieldInputGroup, newOrReturningInputGroup)

type Input =
    { fields :: Array Field
    , fieldValues :: FieldValues
    , newOrReturning :: Boolean
    , ambitions :: String
    , ambitionsError :: Boolean
    }

emptyInput :: Array Field -> Input
emptyInput fields =
    { fields
    , fieldValues: MultiMap.empty
    , newOrReturning: false
    , ambitions: ""
    , ambitionsError: false
    }

type Output =
    { fieldValues :: FieldValues
    , newOrReturning :: Boolean
    , ambitions :: String
    }

type State =
    { fields :: Array Field
    , fieldValues :: FieldValues
    , newOrReturning :: Boolean
    , ambitions :: String
    , ambitionsError :: Boolean
    }

data Action
    = Receive Input
    | UpdateFieldValue String (MultiSelect.Output Option)
    | UpdateNewOrReturning Boolean
    | UpdateAmbitions String

type Slot = H.Slot (Const Void) Output Unit

type ChildSlots = ("multiSelectField" :: MultiSelect.Slot Option String)

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render { fields, fieldValues, newOrReturning, ambitions, ambitionsError } =
    HH.div_ $
    [ inputGroupsHeading "Details"
    , responsiveInputGroups $
        (fields <#> fieldInputGroup fieldValues UpdateFieldValue)
        <>
        [ newOrReturningInputGroup newOrReturning UpdateNewOrReturning ]
    , inputGroupsHeading "Ambitions"
    , ambitionsInputGroup ambitions UpdateAmbitions ambitionsError
    ]

stateToOutput :: State -> Output
stateToOutput state =
    state
    # Record.delete (SProxy :: SProxy "ambitionsError")
    # Record.delete (SProxy :: SProxy "fields")

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive input) =
    H.put input
handleAction (UpdateFieldValue fieldKey (MultiSelect.SelectedChanged options)) = do
    state <- H.modify \state -> state
        { fieldValues =
            case Array.uncons options of
            Nothing -> MultiMap.delete fieldKey state.fieldValues
            Just { head, tail } ->
                MultiMap.insertOrReplace
                fieldKey
                (NonEmptyList $ NonEmpty head.key (List.fromFoldable $ _.key <$> tail))
                state.fieldValues
        }
    H.raise $ stateToOutput state
handleAction (UpdateNewOrReturning newOrReturning) = do
    state <- H.modify _ { newOrReturning = newOrReturning }
    H.raise $ stateToOutput state
handleAction (UpdateAmbitions ambitions) = do
    state <- H.modify _ { ambitions = ambitions }
    H.raise $ stateToOutput state

component :: forall query left. H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

enterProfile
    :: forall children action left
    .  Input
    -> (Output -> action)
    -> HH.ComponentHTML action (enterProfile :: Slot | children) (Async left)
enterProfile input handleMessage =
    HH.slot (SProxy :: SProxy "enterProfile") unit component input (Just <<< handleMessage)
