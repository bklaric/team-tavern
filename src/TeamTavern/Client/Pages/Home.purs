module TeamTavern.Client.Pages.Home where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.RegisterForm as RegisterForm
import TeamTavern.Client.Home.CallToAction (callToAction)
import TeamTavern.Client.Home.Games (games)
import TeamTavern.Client.Home.Games as Games
import TeamTavern.Client.Script.Cookie (PlayerInfo, getPlayerInfo)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)

data Action = Init

data State = Empty | Loaded (Maybe PlayerInfo)

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( games :: Games.Slot Unit
    , registerForm :: RegisterForm.Slot Unit
    )

render :: forall left.
    State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div [ HP.class_ $ HH.ClassName "home" ] []
render (Loaded playerInfo) = HH.div [ HP.class_ $ HH.ClassName "home" ] $
    case playerInfo of
        Nothing -> [ callToAction, games Nothing ]
        Just { nickname } -> [ games $ Just nickname ]

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction Init = do
    H.liftEffect do
        setMetaTitle "Easily find your ideal esports teammates | TeamTavern"
        setMetaDescription $
            "Tired of random matchmaking that results in ruined matches and wasted time? "
            <> "TeamTavern lets you easily find your ideal teammates for the games you play."
        setMetaUrl
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