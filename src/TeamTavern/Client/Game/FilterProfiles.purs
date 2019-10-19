module TeamTavern.Client.Game.FilterProfiles where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.MultiSelect (multiSelectIndexed)
import TeamTavern.Client.Components.MultiSelect as MultiSelect
import Web.Event.Event (preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent, toEvent)

type Option =
    { key :: String
    , option :: String
    }

type Field =
    { key :: String
    , label :: String
    , domain :: Maybe String
    , options :: Array Option
    }

type Input = Array Field

type State = Array Field

data Action = Receive Input | Apply MouseEvent | Clear MouseEvent

data Output = ApplyFilters (Array Field)

type Slot = H.Slot (Const Void) Output

type ChildSlots = (filter :: MultiSelect.Slot Option Field)

fieldLabel :: forall slots action. String -> HH.HTML slots action
fieldLabel label = HH.label
    [ HP.class_ $ HH.ClassName "input-label", HP.for label ] [ HH.text label ]

fieldInput
    :: forall monad
    .  MonadEffect monad
    => Field
    -> H.ComponentHTML Action ChildSlots monad
fieldInput field @ { label, options } =
    HH.div [ HP.class_ $ H.ClassName "filter" ]
    [ fieldLabel label
    , multiSelectIndexed (SProxy :: SProxy "filter") field
        { options: options <#> \option -> { option, selected: false }
        , labeler: _.option
        , comparer: \leftOption rightOption -> leftOption.key == rightOption.key
        }
    ]

render :: forall monad. MonadEffect monad =>
    State -> H.ComponentHTML Action ChildSlots monad
render fields = HH.div [ HP.class_ $ HH.ClassName "card" ]
    [ HH.h3 [ HP.class_ $ HH.ClassName "card-title" ]
        [ HH.text "Profile filters" ]
    , HH.div [ HP.class_ $ HH.ClassName "card-content" ] $
        map fieldInput fields
        <>
        [ HH.button
            [ HP.class_ $ H.ClassName "apply-filters"
            , HE.onClick $ Just <<< Apply
            ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-filter button-icon" ] []
            , HH.text "Apply filters"
            ]
        , HH.button
            [ HP.class_ $ H.ClassName "clear-filters"
            , HE.onClick $ Just <<< Clear
            ]
            [ HH.i [ HP.class_ $ HH.ClassName "fas fa-eraser button-icon" ] []
            , HH.text "Clear filters"
            ]
        ]
    ]

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive fields) =
    H.put fields
handleAction (Apply event) = do
    H.liftEffect $ preventDefault $ toEvent event
    filters <- H.queryAll (SProxy :: SProxy "filter")
        $ MultiSelect.Selected identity
    let (filteredFields :: Array _) = filters
            # Map.toUnfoldable
            <#> \(Tuple field options) -> field { options = options }
    H.raise $ ApplyFilters filteredFields
handleAction (Clear event) = do
    H.liftEffect $ preventDefault $ toEvent event
    -- Clear every multiselect child.
    void $ H.queryAll (SProxy :: SProxy "filter") $ MultiSelect.Clear unit

component :: forall query left.
    H.Component HH.HTML query Input Output (Async left)
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        }
    }

filterProfiles
    :: forall query children left
    .  (Array Field)
    -> (Output -> Maybe query)
    -> HH.ComponentHTML query (filterProfiles :: Slot Unit | children) (Async left)
filterProfiles fields handleOutput =
    HH.slot (SProxy :: SProxy "filterProfiles") unit component fields handleOutput
