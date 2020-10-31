module TeamTavern.Client.Components.Team.ProfileFormInput (Input, Output, Slot, emptyInput, profileFormInput) where

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
import TeamTavern.Client.Components.Input (inputGroupsHeading, responsiveInputGroups)
import TeamTavern.Client.Components.Select.MultiSelect as MultiSelect
import TeamTavern.Client.Components.Team.ProfileInputGroup (Field, FieldValues, Option, ambitionsInputGroup, fieldInputGroup, newOrReturningInputGroup)

type Input =
    { fields :: Array Field
    , fieldValues :: FieldValues
    , newOrReturning :: Boolean
    , ambitions :: String
    , ambitionsError :: Boolean
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

raiseOutput :: forall left. State -> H.HalogenM State Action ChildSlots Output (Async left) Unit
raiseOutput { fieldValues, newOrReturning, ambitions } =
    H.raise { fieldValues, newOrReturning, ambitions }

handleAction :: forall left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive input) =
    H.put input
handleAction (UpdateFieldValue fieldKey options) = do
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
    , fieldValues: MultiMap.empty
    , newOrReturning: false
    , ambitions: ""
    , ambitionsError: false
    }

profileFormInput
    :: forall children action left
    .  Input
    -> (Output -> action)
    -> HH.ComponentHTML action (profileFormInput :: Slot | children) (Async left)
profileFormInput input handleMessage =
    HH.slot (SProxy :: SProxy "profileFormInput") unit component input (Just <<< handleMessage)
