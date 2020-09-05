module TeamTavern.Client.Pages.Home.Wizard where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Pages.Home.Wizard.SelectGame (Game, selectGame)
import TeamTavern.Client.Pages.Home.Wizard.SelectGame as SelectGame
import TeamTavern.Client.Pages.Home.Wizard.Shared (Ilk)
import Unsafe.Coerce (unsafeCoerce)

data Step = SelectGame

type Input = { ilk :: Ilk }

type Output = {}

type State = { step :: Step, ilk :: Ilk }

data Action = TakeSelectedGame Game

type Slot = H.Slot (Modal.Query Input (Const Void)) (Modal.Message Output) Unit

type Slots slots = (selectGame :: SelectGame.Slot | slots)

render :: forall slots left.
    State -> HH.ComponentHTML Action (Slots slots) (Async left)
render { step: SelectGame, ilk } =
    HH.div
    [ HP.class_ $ HH.ClassName "wide-single-form-container" ]
    [ selectGame { ilk } (Just <<< TakeSelectedGame) ]

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction (TakeSelectedGame game) = do
    log $ unsafeCoerce game

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: \{ ilk } -> { step: SelectGame, ilk }
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction }
    }

wizard
    :: forall action slots left
    .  (Modal.Message Output -> Maybe action)
    -> HH.ComponentHTML action (wizard :: Slot | slots) (Async left)
wizard handleOutput = HH.slot
    (SProxy :: SProxy "wizard") unit
    (Modal.component component) unit handleOutput
