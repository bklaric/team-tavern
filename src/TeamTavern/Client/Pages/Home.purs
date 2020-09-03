module TeamTavern.Client.Pages.Home where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Pages.Home.CallToAction (callToAction)
import TeamTavern.Client.Pages.Home.Features (features)
import TeamTavern.Client.Pages.Home.Why (why)
import TeamTavern.Client.Script.Cookie (PlayerInfo, getPlayerInfo)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)

data Action = Init

data State = Empty | Loaded (Maybe PlayerInfo)

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( callToActionButton :: NavigationAnchor.Slot Unit )

render :: forall left.
    State -> H.ComponentHTML Action ChildSlots (Async left)
render Empty = HH.div [ HP.class_ $ HH.ClassName "home" ] []
render (Loaded playerInfo) = HH.div [ HP.class_ $ HH.ClassName "home" ] $
    case playerInfo of
        Nothing -> [ callToAction, why, features ]
        Just { nickname } -> [ ]

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction Init = do
    H.liftEffect do
        setMetaTitle "Find your esports teammates | TeamTavern"
        setMetaDescription $
            "TeamTavern is an online platform for finding esports teammates. "
            <> "Choose a game, browse player and team profiles and find your ideal teammates."
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
