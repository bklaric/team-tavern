module TeamTavern.Client.Home where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.RegisterForm as RegisterForm
import TeamTavern.Client.Components.TopBar (topBar)
import TeamTavern.Client.Components.TopBar as TopBar
import TeamTavern.Client.Home.CallToAction (callToAction)
import TeamTavern.Client.Home.Games (games)
import TeamTavern.Client.Home.Games as Games
import TeamTavern.Client.Script.Cookie (PlayerInfo, getPlayerInfo)
import TeamTavern.Client.Script.Title (setWindowTitle)

data Action = Init

data State = Empty | Loaded (Maybe PlayerInfo)

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( games :: Games.Slot Unit
    , registerForm :: RegisterForm.Slot Unit
    , topBar :: TopBar.Slot Unit
    )

render :: forall left.
    State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div_ []
render (Loaded playerInfo) = HH.div_ $
    [ topBar ]
    <> case playerInfo of
        Nothing -> [ callToAction, games Nothing ]
        Just { nickname } -> [ games $ Just nickname ]

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction Init = do
    H.liftEffect $
        setWindowTitle "Easily find your ideal esports teammates | TeamTavern"
    playerInfo <- H.liftEffect $ getPlayerInfo
    H.put $ Loaded playerInfo

component :: forall query input output left.
    H.Component HH.HTML query input output (Async left)
component = H.mkComponent
    { initialState: const Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        }
    }

home :: forall query children left.
    HH.ComponentHTML query (home :: Slot Unit | children) (Async left)
home = HH.slot (SProxy :: SProxy "home") unit component unit absurd
