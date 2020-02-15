module TeamTavern.Client.Profile.ProfileFilters where

import Prelude

import Async (Async)
import Control.Bind (bindFlipped)
import Data.Const (Const)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.CheckboxInput as CheckboxInput
import TeamTavern.Client.Components.MultiSelect (multiSelect, multiSelectIndexed)
import TeamTavern.Client.Components.MultiSelect as MultiSelect
import TeamTavern.Server.Infrastructure.Languages (allLanguages)
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Filters =
    { age :: { from :: Maybe Int, to :: Maybe Int }
    , languages :: Array String
    , microphone :: Boolean
    , fields :: Array Field
    }

type Option =
    { key :: String
    , option :: String
    }

type Field =
    { key :: String
    , label :: String
    , icon :: String
    , domain :: Maybe String
    , options :: Array Option
    }

type Input = Array Field

type State = Array Field

data Action = Receive Input | Apply MouseEvent | Clear MouseEvent

data Output = ApplyFilters Filters

type Slot = H.Slot (Const Void) Output

type ChildSlots =
    ( language :: MultiSelect.Slot String Unit
    , microphone :: CheckboxInput.Slot
    , filter :: MultiSelect.Slot Option Field
    )

fieldLabel :: forall slots action. String -> String -> HH.HTML slots action
fieldLabel label icon = HH.label
    [ HP.class_ $ HH.ClassName "input-label", HP.for label ]
    [ HH.i [ HP.class_ $ HH.ClassName $ icon <> " filter-field-icon" ] []
    , HH.span [ HP.class_ $ HH.ClassName "filter-field-label" ] [ HH.text label ]
    ]

fieldInput
    :: forall monad
    .  MonadEffect monad
    => Field
    -> H.ComponentHTML Action ChildSlots monad
fieldInput field @ { label, icon, options } =
    HH.div [ HP.class_ $ HH.ClassName "input-group" ]
    [ fieldLabel label icon
    , multiSelectIndexed (SProxy :: SProxy "filter") field
        { options: options <#> { option: _, selected: false }
        , labeler: _.option
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        , showFilter: Nothing
        }
    ]

render :: forall monad. MonadEffect monad =>
    State -> H.ComponentHTML Action ChildSlots monad
render fields = HH.div [ HP.class_ $ HH.ClassName "card" ]
    [ HH.span [ HP.class_ $ HH.ClassName "card-title" ]
        [ HH.text "Profile filters" ]
    , HH.div [ HP.class_ $ HH.ClassName "card-content" ]
        [ HH.div [ HP.class_ $ HH.ClassName "responsive-input-groups" ] $
            [ HH.div [ HP.class_ $ HH.ClassName "input-group" ]
                [ fieldLabel "Age" "fas fa-calendar-alt"
                , HH.div [ HP.class_ $ HH.ClassName "timespan-group" ]
                    [ HH.span [ HP.class_ $ HH.ClassName "timespan-group-from" ] [ HH.text "From" ]
                    , HH.input
                        [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                        , HP.ref $ H.RefLabel "ageFrom"
                        , HP.type_ HP.InputNumber
                        ]
                    , HH.span [ HP.class_ $ HH.ClassName "timespan-group-to" ] [ HH.text "to" ]
                    , HH.input
                        [ HP.class_ $ HH.ClassName $ "text-line-input timespan-group-input"
                        , HP.ref $ H.RefLabel "ageTo"
                        , HP.type_ HP.InputNumber
                        ]
                    ]
                ]
            , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
                [ fieldLabel "Language" "fas fa-comments"
                , multiSelect (SProxy :: SProxy "language")
                    { options: allLanguages <#> { option: _, selected: false }
                    , labeler: identity
                    , comparer: (==)
                    , showFilter: Just "Search languages"
                    }
                ]
            , HH.div [ HP.class_ $ HH.ClassName "input-group" ]
                [ fieldLabel "Microphone" "fas fa-microphone"
                , HH.label
                    [ HP.class_ $ HH.ClassName "checkbox-input-label" ]
                    [ HH.input
                        [ HP.class_ $ HH.ClassName "checkbox-input"
                        , HP.type_ HP.InputCheckbox
                        , HP.ref $ H.RefLabel "microphone"
                        ]
                    , HH.text "Must have a microphone and be willing to communicate."
                    ]
                ]
            ]
            <>
            (map fieldInput fields)
        , HH.button
            [ HP.class_ $ HH.ClassName "apply-filters"
            , HE.onClick $ Just <<< Apply
            ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-filter button-icon" ] []
            , HH.text "Apply filters"
            ]
        , HH.button
            [ HP.class_ $ HH.ClassName "clear-filters"
            , HE.onClick $ Just <<< Clear
            ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-eraser button-icon" ] []
            , HH.text "Clear filters"
            ]
        ]
    ]

getAge :: forall state action slots message monad. MonadEffect monad =>
    H.RefLabel -> H.HalogenM state action slots message monad (Maybe Int)
getAge label = do
    input <- H.getRef label
    input
        >>= HTMLInputElement.fromElement
        # traverse HTMLInputElement.value
        <#> bindFlipped Int.fromString
        # H.liftEffect

clearAge :: forall state action slots message monad. MonadEffect monad =>
    H.RefLabel -> H.HalogenM state action slots message monad Unit
clearAge label = do
    input <- H.getRef label
    input' <- input >>= HTMLInputElement.fromElement # pure
    case input' of
        Nothing -> pure unit
        Just input'' -> H.liftEffect $ HTMLInputElement.setValue "" input''

getMicrophone :: forall state action slots message monad. MonadEffect monad =>
    H.HalogenM state action slots message monad (Maybe Boolean)
getMicrophone = do
    input <- H.getRef $ H.RefLabel "microphone"
    input
        >>= HTMLInputElement.fromElement
        # traverse HTMLInputElement.checked
        # H.liftEffect

clearMicrophone :: forall state action slots message monad. MonadEffect monad =>
    H.HalogenM state action slots message monad Unit
clearMicrophone = do
    input <- H.getRef $ H.RefLabel "microphone"
    input' <- input >>= HTMLInputElement.fromElement # pure
    case input' of
        Nothing -> pure unit
        Just input'' -> H.liftEffect $ HTMLInputElement.setChecked false input''

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive fields) =
    H.put fields
handleAction (Apply event) = do
    H.liftEffect $ preventDefault $ toEvent event
    ageFrom <- getAge $ H.RefLabel "ageFrom"
    ageTo <- getAge $ H.RefLabel "ageTo"
    microphone <- getMicrophone
    languages <- H.query (SProxy :: SProxy "language") unit
        $ MultiSelect.Selected identity
    filters <- H.queryAll (SProxy :: SProxy "filter")
        $ MultiSelect.Selected identity
    let (filteredFields :: Array _) = filters
            # Map.toUnfoldable
            <#> \(Tuple field options) -> field { options = options }
    H.raise $ ApplyFilters
        { age: { from: ageFrom, to: ageTo }
        , languages: maybe [] identity languages
        , microphone: maybe false identity microphone
        , fields: filteredFields
        }
handleAction (Clear event) = do
    H.liftEffect $ preventDefault $ toEvent event
    clearAge $ H.RefLabel "ageFrom"
    clearAge $ H.RefLabel "ageTo"
    clearMicrophone
    void $ H.query (SProxy :: SProxy "language") unit $ MultiSelect.Clear unit
    -- Clear every multiselect child.
    void $ H.queryAll (SProxy :: SProxy "filter") $ MultiSelect.Clear unit

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

filterProfiles
    :: forall query children left
    .  (Array Field)
    -> (Output -> Maybe query)
    -> HH.ComponentHTML query (filterProfiles :: Slot Unit | children) (Async left)
filterProfiles fields handleOutput =
    HH.slot (SProxy :: SProxy "filterProfiles") unit component fields handleOutput
