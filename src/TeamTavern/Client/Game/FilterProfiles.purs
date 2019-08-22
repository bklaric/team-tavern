module TeamTavern.Client.Game.FilterProfiles where

import Prelude

import Async (Async)
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
    { id :: Int
    , key :: String
    , option :: String
    }

type Field =
    { id :: Int
    , key :: String
    , label :: String
    , options :: Array Option
    }

type Input = Array Field

type State =
    { fields :: Array Field
    , visible :: Boolean
    }

data Action = Receive Input | Apply MouseEvent

data Query send = ToggleFilterProfiles send

data Output = ApplyFilters (Array Field)

type Slot = H.Slot Query Output

type ChildSlots = ( "filter" :: MultiSelect.Slot Option Field )

fieldLabel :: forall slots action. String -> HH.HTML slots action
fieldLabel label = HH.label [ HP.for label ] [ HH.text label ]

fieldInput
    :: forall monad
    .  Field
    -> H.ComponentHTML Action ChildSlots monad
fieldInput field @ { label, options } =
    HH.div [ HP.class_ $ H.ClassName "filter" ]
    [ fieldLabel label
    , multiSelectIndexed (SProxy :: SProxy "filter") field
        { options: options <#> \option -> { option, selected: false }
        , labeler: _.option
        , comparer: \leftOption rightOption -> leftOption.id == rightOption.id
        }
    ]

render :: forall monad. State -> H.ComponentHTML Action ChildSlots monad
render ({ visible: false }) =
    HH.div_ []
render ({ fields, visible: true }) = HH.div
    [ HP.class_ $ H.ClassName "card" ]
    $ map fieldInput fields
    <>
    [ HH.button
        [ HP.class_ $ H.ClassName "primary"
        , HE.onClick $ Just <<< Apply
        ]
        [ HH.text "Apply filters" ]
    ]

handleAction :: forall left.
    Action -> H.HalogenM State Action ChildSlots Output (Async left) Unit
handleAction (Receive fields) =
    H.put { fields, visible: false }
handleAction (Apply event) = do
    H.liftEffect $ preventDefault $ toEvent event
    filters <- H.queryAll (SProxy :: SProxy "filter")
        $ MultiSelect.Selected identity
    let (filteredFields :: Array _) = filters
            # Map.toUnfoldable
            <#> \(Tuple field options) -> field { options = options }
    H.raise $ ApplyFilters filteredFields

handleQuery
    :: forall output monad send
    .  MonadEffect monad
    => Query send
    -> H.HalogenM State Action ChildSlots output monad (Maybe send)
handleQuery (ToggleFilterProfiles send) = do
    H.modify_ \state -> state { visible = not state.visible }
    pure $ Just send

component :: forall left.
    H.Component HH.HTML Query Input Output (Async left)
component = H.mkComponent
    { initialState: { fields: _, visible: false }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleQuery = handleQuery
        , handleAction = handleAction
        }
    }

filterProfiles
    :: forall query children left
    .  (Array Field)
    -> (Output -> Maybe query)
    -> HH.ComponentHTML query (filterProfiles :: Slot Unit | children) (Async left)
filterProfiles fields handleOutput =
    HH.slot (SProxy :: SProxy "filterProfiles") unit component fields handleOutput
