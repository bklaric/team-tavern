module TeamTavern.Client.Pages.Home where

import Prelude

import Async (Async)
import Client.Pages.Home.ForTeams (forTeams)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.Ads (AdSlots, billboard, leaderboard, mobileMpu, mobileTakeover)
import TeamTavern.Client.Components.Boarding.PlayerOrTeamInput as Boarding
import TeamTavern.Client.Pages.Home.CallToAction (callToAction)
import TeamTavern.Client.Pages.Home.Connect (connect)
import TeamTavern.Client.Pages.Home.Features (features)
import TeamTavern.Client.Pages.Home.FindProfiles (findProfiles)
import TeamTavern.Client.Pages.Home.ForPlayers (forPlayers)
import TeamTavern.Client.Pages.Home.Games (games)
import TeamTavern.Client.Pages.Preboarding as Preboarding
import TeamTavern.Client.Script.Meta (setMeta)
import TeamTavern.Client.Script.Navigate (navigate)
import TeamTavern.Client.Script.Scroll (scrollToId)
import TeamTavern.Client.Shared.Slot (Slot___)
import TeamTavern.Client.Snippets.PreventMouseDefault (preventMouseDefault)
import Type.Proxy (Proxy(..))
import Web.UIEvent.MouseEvent (MouseEvent)

data Action
    = Initialize
    | OpenPreboarding MouseEvent
    | OpenPlayerPreboarding MouseEvent
    | OpenTeamPreboarding MouseEvent
    | ScrollToGames MouseEvent

type State = Unit

type ChildSlots = AdSlots
    ( games :: Slot___
    )

render :: ∀ left. State -> H.ComponentHTML Action ChildSlots (Async left)
render _ =
    HH.div [ HP.class_ $ HH.ClassName "home" ] $
    [ callToAction ScrollToGames OpenPreboarding
    , games
    , forPlayers OpenPlayerPreboarding
    , billboard
    , mobileTakeover
    , forTeams OpenTeamPreboarding
    , findProfiles ScrollToGames
    , leaderboard
    , mobileMpu
    , connect
    , features OpenPreboarding
    ]

handleAction :: ∀ action output slots left.
    Action -> H.HalogenM State action slots output (Async left) Unit
handleAction Initialize = setMeta
    "Esports Team Finder / LFG / LFT / LFM / LFP | TeamTavern"
    ( "Find esports players and teams looking for teammates on TeamTavern, an esports team finding platform. "
    <> "Create your own player or team profile and let them find you."
    )
handleAction (OpenPreboarding mouseEvent) = do
    preventMouseDefault mouseEvent
    navigate (Preboarding.emptyInput Nothing Nothing) "/preboarding/start"
handleAction (OpenPlayerPreboarding mouseEvent) = do
    preventMouseDefault mouseEvent
    navigate (Preboarding.emptyInput (Just Boarding.Player) Nothing) "/preboarding/start"
handleAction (OpenTeamPreboarding mouseEvent) = do
    preventMouseDefault mouseEvent
    navigate (Preboarding.emptyInput (Just Boarding.Team) Nothing) "/preboarding/start"
handleAction (ScrollToGames mouseEvent) = do
    preventMouseDefault mouseEvent
    scrollToId "games"

component :: ∀ query input output left. H.Component query input output (Async left)
component = H.mkComponent
    { initialState: const unit
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

home :: ∀ query children left.
    HH.ComponentHTML query (home :: Slot___ | children) (Async left)
home = HH.slot (Proxy :: _ "home") unit component unit absurd
