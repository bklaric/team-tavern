module TeamTavern.Client.Pages.Home where

import Prelude

import Async (Async)
import Client.Pages.Home.ForTeams (forTeams)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Boarding.PlayerOrTeamInput as Boarding
import TeamTavern.Client.Components.NavigationAnchor as NavigationAnchor
import TeamTavern.Client.Pages.Home.CallToAction (callToAction)
import TeamTavern.Client.Pages.Home.Features (features)
import TeamTavern.Client.Pages.Home.FindProfiles (findProfiles)
import TeamTavern.Client.Pages.Home.ForPlayers (forPlayers)
import TeamTavern.Client.Pages.Preboarding as Preboarding
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Navigate (navigate, navigate_)

data Action
    = Initialize
    | OpenPreboarding
    | OpenPlayerPreboarding
    | OpenTeamPreboarding
    | OpenGames

type State = Unit

type Slot = H.Slot (Const Void) Void

type ChildSlots =
    ( viewAllGames :: NavigationAnchor.Slot Unit
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render _ =
    HH.div [ HP.class_ $ HH.ClassName "home" ]
    [ callToAction Nothing OpenPreboarding
    , forPlayers OpenPlayerPreboarding
    , forTeams OpenTeamPreboarding
    , findProfiles OpenGames
    , features OpenPreboarding
    ]

handleAction :: forall action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction Initialize =
    H.liftEffect do
        setMetaTitle "Find your esports teammates | TeamTavern"
        setMetaDescription $
            "TeamTavern is an online platform for finding esports teammates. "
            <> "Choose a game, browse player and team profiles and find your ideal teammates."
        setMetaUrl
handleAction OpenPreboarding =
    navigate (Preboarding.emptyInput Nothing Nothing) "/preboarding/start"
handleAction OpenPlayerPreboarding =
    navigate (Preboarding.emptyInput (Just Boarding.Player) Nothing) "/preboarding/start"
handleAction OpenTeamPreboarding =
    navigate (Preboarding.emptyInput (Just Boarding.Team) Nothing) "/preboarding/start"
handleAction OpenGames = navigate_ "/games"

component :: forall query input output left. H.Component HH.HTML query input output (Async left)
component = H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

home :: forall query children left.
    HH.ComponentHTML query (home :: Slot Unit | children) (Async left)
home = HH.slot (SProxy :: SProxy "home") unit component unit absurd
