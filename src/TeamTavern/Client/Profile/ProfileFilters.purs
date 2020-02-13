module TeamTavern.Client.Profile.ProfileFilters where

import Prelude

import Async (Async)
import Control.Bind (bindFlipped)
import Data.Const (Const)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.MultiSelect (multiSelectIndexed)
import TeamTavern.Client.Components.MultiSelect as MultiSelect
import Web.Event.Event (preventDefault)
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

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

data Output = ApplyFilters { from :: Maybe Int, to :: Maybe Int } (Array Field)

type Slot = H.Slot (Const Void) Output

type ChildSlots =
    ( filter :: MultiSelect.Slot Option Field
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
        { options: options <#> \option -> { option, selected: false }
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

getAge label = do
    input <- H.getRef label
    input
        >>= HTMLInputElement.fromElement
        # traverse HTMLInputElement.value
        <#> bindFlipped Int.fromString
        # H.liftEffect

clearAge label = do
    input <- H.getRef label
    input' <- input >>= HTMLInputElement.fromElement # pure
        -- # traverse HTMLInputElement.value
        -- <#> bindFlipped Int.fromString
        -- # H.liftEffect
    case input' of
        Nothing -> pure unit
        Just input'' -> H.liftEffect $ HTMLInputElement.setValue "" input''

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive fields) =
    H.put fields
handleAction (Apply event) = do
    H.liftEffect $ preventDefault $ toEvent event
    ageFrom <- getAge $ H.RefLabel "ageFrom"
    ageTo <- getAge $ H.RefLabel "ageTo"
    filters <- H.queryAll (SProxy :: SProxy "filter")
        $ MultiSelect.Selected identity
    let (filteredFields :: Array _) = filters
            # Map.toUnfoldable
            <#> \(Tuple field options) -> field { options = options }
    H.raise $ ApplyFilters { from: ageFrom, to: ageTo } filteredFields
handleAction (Clear event) = do
    H.liftEffect $ preventDefault $ toEvent event
    clearAge $ H.RefLabel "ageFrom"
    clearAge $ H.RefLabel "ageTo"
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
