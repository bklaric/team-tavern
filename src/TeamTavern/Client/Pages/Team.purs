module TeamTavern.Client.Pages.Team where

import Prelude

import Async (Async)
import Async as Async
import Client.Components.Copyable as Copyable
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen (ClassName(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Pages.Player.Teams (get)
import TeamTavern.Client.Pages.Team.Details (details)
import TeamTavern.Client.Pages.Team.Profiles (profiles)
import TeamTavern.Client.Script.Meta (setMetaDescription, setMetaTitle, setMetaUrl)
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Server.Team.View (Team)

type Input = { handle :: String }

data State
    = Empty Input
    | Loaded { team :: Team }
    | NotFound
    | Error

data Action = Initialize

type Slot = H.Slot (Const Void) Void Unit

type ChildSlots =
    ( discordServer :: Copyable.Slot
    , games :: Anchor.Slot String
    )

render :: forall left. State -> H.ComponentHTML Action ChildSlots (Async left)
render (Empty _) = HH.div_ []
render (Loaded { team: team' } ) =
    HH.div_  $
    [ HH.div [ HP.class_ $ ClassName "content-title" ]
        [ HH.div [ HP.class_ $ HH.ClassName "content-title-left"]
            [ HH.h1 [ HP.class_ $ HH.ClassName "content-title-text" ]
                [ HH.text team'.name ]
            ]
        ]
    , HH.p [ HP.class_ $ HH.ClassName "content-description" ]
        [ HH.text "This is a team, lmao!" ]
    , details team'
    , profiles team'.profiles
    ]
render NotFound = HH.p_ [ HH.text "Team could not be found." ]
render Error = HH.p_ [ HH.text "There has been an error loading the team. Please try again later." ]

loadTeam :: forall left. String -> Async left (Maybe Team)
loadTeam handle = do
    timezone <- H.liftEffect getClientTimezone
    get $ "/api/teams/by-handle/" <> handle <> "?timezone=" <> timezone

handleAction :: forall output left.
    Action -> H.HalogenM State Action ChildSlots output (Async left) Unit
handleAction Initialize = do
    state <- H.get
    case state of
        Empty { handle } -> do
            team' <- H.lift $ loadTeam handle
            case team' of
                Just team'' -> H.put $ Loaded { team: team'' }
                _ -> pure unit
        _ -> pure unit
    H.lift $ Async.fromEffect do
        setMetaTitle $ "aoeu" <> " | TeamTavern"
        setMetaDescription $ "View profiles by player " <> "aoeueuue" <> " on TeamTavern."
        setMetaUrl

component :: forall query output left.
    H.Component HH.HTML query Input output (Async left)
component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

team :: forall query children left.
    Input -> HH.ComponentHTML query (team :: Slot | children) (Async left)
team handle = HH.slot (SProxy :: SProxy "team") unit component handle absurd
