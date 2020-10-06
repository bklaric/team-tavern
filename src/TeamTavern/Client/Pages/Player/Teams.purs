module TeamTavern.Client.Pages.Player.Teams where

import Prelude

import Async (Async)
import Async as Async
import Browser.Async.Fetch as Fetch
import Browser.Async.Fetch.Response as FetchRes
import Data.Array (intercalate)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Halogen (defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Prim.Row (class Cons)
import Simple.JSON.Async as Json
import TeamTavern.Client.Components.Divider (divider)
import TeamTavern.Client.Components.Modal as Modal
import TeamTavern.Client.Components.NavigationAnchor (navigationAnchorIndexed)
import TeamTavern.Client.Components.NavigationAnchor as Anchor
import TeamTavern.Client.Pages.Player.EditTeamProfile (editTeamProfile)
import TeamTavern.Client.Pages.Player.EditTeamProfile as EditProfile
import TeamTavern.Client.Pages.Player.Types (Nickname, PlayerStatus(..))
import TeamTavern.Client.Script.Timezone (getClientTimezone)
import TeamTavern.Client.Snippets.Class as HS
import TeamTavern.Server.Profile.ViewTeamProfilesByPlayer.SendResponse as ViewTeamProfilesByPlayer

type Team = { handle :: String, name :: String }

type Input = { nickname :: String, status :: PlayerStatus }

data State = Empty Input | Loaded (Array Team)

data Action = Initialize

type Slot = H.Slot (Const Void) Void Unit

renderTeams (teams' :: Array { handle :: String, name :: String }) =
    teams' <#> \team ->
        HH.div [ HS.class_ "card-section" ]
        [ HH.h3 [ HS.class_ "team-heading" ]
            [ navigationAnchorIndexed (SProxy :: SProxy "team") team.handle
                { path: "/teams/" <> team.handle
                , content: HH.text team.name
                }
            , divider
            , HH.span [ HP.class_ $ HH.ClassName "profile-updated" ]
                [ HH.text $ "Updated 12 days ago" ]
            ]
        ]

render (Empty _) = HH.div_ []
render (Loaded teams') =
    HH.div [ HS.class_ "card" ] $
    [ HH.h2 [ HS.class_ "card-title" ] [ HH.text "Teams" ] ]
    <> renderTeams teams'

handleAction Initialize =
    H.put $ Loaded
        [ { handle: "niggaz", name: "Niggaz from da hood" }
        , { handle: "cunts", name: "Aussie cunts" }
        ]

component = H.mkComponent
    { initialState: Empty
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

teams input = HH.slot (SProxy :: SProxy "teams") unit component input absurd
