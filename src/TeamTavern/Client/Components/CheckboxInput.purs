module TeamTavern.Client.Components.CheckboxInput
    (Value, Query(..), Slot, checkboxInput) where

import Prelude

import Control.Monad.State (class MonadState)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (SProxy)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)

type Id = String

type Value = Boolean

type State = { id :: Id, value :: Value }

data Action = ValueInput Value

data Query send = GetValue (Value -> send)

type Slot = H.Slot Query Void Unit

render :: forall slots. State -> HH.HTML slots Action
render { id, value } =
    HH.input
    [ HP.id_ id
    , HP.class_ $ HH.ClassName "checkbox-input"
    , HP.type_ HP.InputCheckbox
    , HE.onChecked $ Just <<< ValueInput
    , HP.checked value
    ]

handleAction :: forall monad. MonadState State monad => Action -> monad Unit
handleAction (ValueInput value) = H.modify_ (_ { value = value })

handleQuery :: forall monad send. Bind monad => MonadState State monad =>
    Query send -> monad (Maybe send)
handleQuery (GetValue send) = do
    { value } <- H.get
    pure $ Just $ send value

component :: forall t44 t45. H.Component HH.HTML Query State t45 t44
component = H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
    }

checkboxInput
    :: forall children' label monad children action
    .  Cons label Slot children' children
    => IsSymbol label
    => SProxy label
    -> State
    -> HH.ComponentHTML action children monad
checkboxInput label state = HH.slot label unit component state absurd
