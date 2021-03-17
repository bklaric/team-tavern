module TeamTavern.Client.Components.Team.ProfileFormInput (FieldValues, Input, Output, Slot, emptyInput, profileFormInput) where

import Prelude

import Async (Async)
import Data.Array (foldl)
import Data.Array as Array
import Data.Const (Const)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.MultiMap as MultiMap
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Record as Record
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Input (inputErrorSublabel, inputGroup, inputGroupsHeading, inputGroupsHeading', inputRequiredSublabel, inputSublabel, platformCheckboxes, responsiveInputGroups)
import TeamTavern.Client.Components.Select.MultiSelect as MultiSelect
import TeamTavern.Client.Components.Team.ProfileInputGroup (Field, Option, ambitionsInputGroup, fieldInputGroup, newOrReturningInputGroup)
import TeamTavern.Client.Components.Team.ProfileInputGroup as Input
import TeamTavern.Client.Components.Team.SizeInfo (sizeInfo)
import TeamTavern.Client.Components.Team.SizeInfo as SizeInfo
import TeamTavern.Client.Pages.Profiles.TeamBadge (teamSizeRadios)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Routes.Shared.Platform (Platform, Platforms)
import TeamTavern.Routes.Shared.Size (Size(..))

type FieldValues = Array
    { fieldKey :: String
    , optionKeys :: Array String
    }

type Input =
    { size :: Size
    , allPlatforms :: Platforms
    , selectedPlatforms :: Array Platform
    , platformsError :: Boolean
    , fields :: Array Field
    , fieldValues :: FieldValues
    , newOrReturning :: Boolean
    , ambitions :: String
    , ambitionsError :: Boolean
    }

type Output =
    { size :: Size
    , platforms :: Array Platform
    , fieldValues :: FieldValues
    , newOrReturning :: Boolean
    , ambitions :: String
    }

type State =
    { size :: Size
    , allPlatforms :: Platforms
    , selectedPlatforms :: Array Platform
    , platformsError :: Boolean
    , fields :: Array Field
    , fieldValues :: Input.FieldValues
    , newOrReturning :: Boolean
    , ambitions :: String
    , ambitionsError :: Boolean
    }

data Action
    = Receive Input
    | UpdateSize Size
    | UpdatePlatform Platform
    | UpdateFieldValues String (MultiSelect.Output Option)
    | UpdateNewOrReturning Boolean
    | UpdateAmbitions String

type Slot = H.Slot (Const Void) Output Unit

type ChildSlots =
    ( "multiSelectField" :: MultiSelect.Slot Option String
    , "sizeInfo" :: SizeInfo.Slot
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render state =
    HH.div_ $
    [ HH.h2 [ HS.class_ "platform-id-heading" ]
        [ HH.text "Size"
        , teamSizeRadios state.size UpdateSize
        , sizeInfo
        ]
    ]
    <> guard (not $ Array.null state.allPlatforms.tail)
    [ inputGroupsHeading' $
        [ HH.text "Platforms"
        , divider, inputRequiredSublabel
        , divider, (if state.platformsError then inputErrorSublabel else inputSublabel)
            "You must select at least one of the available platforms."
        ]
    , inputGroup [ platformCheckboxes state.allPlatforms state.selectedPlatforms UpdatePlatform ]
    ]
    <>
    [ inputGroupsHeading "Details"
    , responsiveInputGroups $
        (state.fields <#> fieldInputGroup state.fieldValues UpdateFieldValues)
        <>
        [ newOrReturningInputGroup state.newOrReturning UpdateNewOrReturning ]
    , inputGroupsHeading "Ambitions"
    , ambitionsInputGroup state.ambitions UpdateAmbitions state.ambitionsError
    ]

fieldValuesToArray :: Input.FieldValues -> FieldValues
fieldValuesToArray = (MultiMap.toUnfoldable' :: _ -> Array (Tuple _ (Array _))) >>>
    map \(Tuple fieldKey optionKeys) -> { fieldKey, optionKeys }

fieldValuesToMap :: FieldValues -> Input.FieldValues
fieldValuesToMap =
    foldl
    (\fieldValues { fieldKey, optionKeys } ->
        case NonEmptyList.fromFoldable optionKeys of
        Nothing -> fieldValues
        Just optionKeys' -> MultiMap.insertOrReplace fieldKey optionKeys' fieldValues
    )
    MultiMap.empty

raiseOutput :: forall left. State -> H.HalogenM State Action ChildSlots Output (Async left) Unit
raiseOutput { size, selectedPlatforms, fieldValues, newOrReturning, ambitions } =
    H.raise
    { size
    , platforms: selectedPlatforms
    , fieldValues: fieldValuesToArray fieldValues
    , newOrReturning
    , ambitions
    }

handleAction :: forall left. Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive input) =
    H.put (Record.modify (SProxy :: SProxy "fieldValues") fieldValuesToMap input)
handleAction (UpdateSize size) = do
    state <- H.modify _ { size = size }
    raiseOutput state
handleAction (UpdatePlatform platform) = do
    state <- H.modify \state -> state
        { selectedPlatforms =
            if Array.elem platform state.selectedPlatforms
            then Array.delete platform state.selectedPlatforms
            else Array.cons platform state.selectedPlatforms
        }
    raiseOutput state
handleAction (UpdateFieldValues fieldKey options) = do
    state <- H.modify \state -> state
        { fieldValues =
            case NonEmptyList.fromFoldable options of
            Nothing -> MultiMap.delete fieldKey state.fieldValues
            Just options' -> MultiMap.insertOrReplace fieldKey (_.key <$> options') state.fieldValues
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

emptyInput :: { platforms :: Platforms, fields :: Array Field } -> Input
emptyInput { platforms, fields } =
    { size: Party
    , allPlatforms: platforms
    , selectedPlatforms: [ platforms.head ]
    , platformsError: false
    , fields
    , fieldValues: []
    , newOrReturning: false
    , ambitions: ""
    , ambitionsError: false
    }

profileFormInput
    :: forall children action left
    .  Input
    -> (Output -> action)
    -> HH.ComponentHTML action (teamProfileFormInput :: Slot | children) (Async left)
profileFormInput input handleMessage =
    HH.slot (SProxy :: SProxy "teamProfileFormInput") unit component input (Just <<< handleMessage)
