module TeamTavern.Client.Pages.GameTabs where

import Prelude

import Async (Async)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH
import TeamTavern.Client.Components.Ads (stickyLeaderboards)
import TeamTavern.Client.Pages.Competitions (competitions)
import TeamTavern.Client.Pages.Competitions as Competitions
import TeamTavern.Client.Pages.Profiles (profiles)
import TeamTavern.Client.Pages.Profiles as Profiles
import TeamTavern.Client.Pages.Profiles.GameHeader (ProfileTab(..), Tab(..), gameHeader)
import TeamTavern.Client.Script.Request (get)
import TeamTavern.Routes.ViewGame as ViewGame

type Input = { handle :: String, tab :: Tab }

data State
    = Empty Input
    | Loaded { game :: ViewGame.OkContent, tab :: Tab }
    | Error

data Action = Initialize | Receive Input

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots =
    ( profiles :: Profiles.Slot
    , competitions :: Competitions.Slot
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Loaded { game, tab }) = HH.div_ $
    [ gameHeader { title: game.title, shortTitle: game.shortTitle, tab }
    , case tab of
        Profiles Players -> profiles { game, tab: Players }
        Profiles Teams -> profiles { game, tab: Teams }
        Competitions -> competitions { game }
    ]
    <> stickyLeaderboards
render Error = HH.p_ [ HH.text "There has been an error loading the game. Please try again later." ]

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state of
        Empty input -> handleAction $ Receive input
        _ -> pure unit
handleAction (Receive input) = do
    state <- H.get
    case state of
        Loaded loaded | loaded.game.handle == input.handle ->
            H.put $ Loaded loaded { tab = input.tab }
        _ -> do
            game' <- H.lift $ get $ "/api/games/" <> input.handle
            H.put case game' of
                Nothing -> Error
                Just game -> Loaded { game, tab: input.tab }

component :: forall query output left. H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        , receive = Just <<< Receive
        }
    }

gameTabs :: forall query children left.
    Input -> HH.ComponentHTML query (gameTabs :: Slot | children) (Async left)
gameTabs input = HH.slot (SProxy :: SProxy "gameTabs") unit component input absurd